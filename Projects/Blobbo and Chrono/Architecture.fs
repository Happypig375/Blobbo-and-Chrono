namespace BlobboAndChrono

open System
open System.Collections.Concurrent
open System.Threading

/// Events crossing the browser boundary. Platform adapters translate these to and from
/// an external browser/extension (desktop) or an owned mobile browser surface.
type BrowserEvent =
    | Navigate of string
    | Play
    | Pause
    | Seek of int64
    | AudioFormat of sampleRate: int * channels: int
    | End
    | Overlay of bounds: struct (int * int * int * int) * visible: bool * focused: bool

type IBrowserBridge =
    abstract Start : unit -> unit
    abstract Stop : unit -> unit
    abstract Send : BrowserEvent -> unit
    /// Non-blocking audio pull; samples are mono 48 kHz analysis samples.
    abstract TryReadAudio : unit -> (float32 array * int * int) option
    abstract Events : IEvent<BrowserEvent>

type NullBrowserBridge () =
    let events = Event<BrowserEvent>()
    interface IBrowserBridge with
        member _.Start () = ()
        member _.Stop () = ()
        member _.Send _ = ()
        member _.TryReadAudio () = None
        member _.Events = events.Publish

/// A bounded SPSC buffer. The callback only copies into preallocated storage and advances
/// the source clock; producers drop new blocks when full (the clock still advances).
type PcmRingBuffer (capacity: int, blockSize: int) =
    let blocks = Array.init capacity (fun _ -> Array.zeroCreate<float32> blockSize)
    let mutable writeIndex = 0
    let mutable readIndex = 0
    let positions = Array.zeroCreate<int64> capacity
    let counts = Array.zeroCreate<int> capacity
    member _.Capacity = capacity
    member _.TryWrite (source: float32 array, offset: int, count: int, position: int64) =
        if count > blockSize then false
        else
            let w = Volatile.Read &writeIndex
            let r = Volatile.Read &readIndex
            if w - r >= capacity then false
            else
                let target = blocks[w % capacity]
                Array.Clear(target, 0, target.Length)
                Array.Copy(source, offset, target, 0, count)
                counts[w % capacity] <- count
                positions[w % capacity] <- position
                Volatile.Write(&writeIndex, w + 1)
                true
    member _.TryRead (destination: float32 array, count: byref<int>, position: byref<int64>) =
        let r = Volatile.Read &readIndex
        if r >= Volatile.Read &writeIndex then false
        else
            let source = blocks[r % capacity]
            Array.Copy(source, destination, min destination.Length blockSize)
            count <- counts[r % capacity]
            position <- positions[r % capacity]
            Volatile.Write(&readIndex, r + 1)
            true

type AudioIngress (analysis: PcmRingBuffer, playback: PcmRingBuffer) =
    let mutable sourceClock = 0L
    member _.SourceSampleClock = Volatile.Read &sourceClock
    member _.Submit (pcm: float32 array, offset: int, count: int) =
        // Both copies are bounded and use preallocated ring slots. Clock advances even if a
        // consumer is behind, making it the authoritative timeline for all derived events.
        let position = Interlocked.Add(&sourceClock, int64 count) - int64 count
        let mutable consumed = 0
        while consumed < count do
            let chunk = min 2048 (count - consumed)
            analysis.TryWrite(pcm, offset + consumed, chunk, position + int64 consumed) |> ignore
            playback.TryWrite(pcm, offset + consumed, chunk, position + int64 consumed) |> ignore
            consumed <- consumed + chunk
    /// Non-blocking drain for the delayed playback/output adapter.
    member _.TryReadPlayback (destination: float32 array, count: byref<int>, position: byref<int64>) =
        playback.TryRead(destination, &count, &position)

type SymbolicEvent = { SamplePosition: int64; Label: string }
type InferenceOptions = { PreludeForcing: bool; Beam: int; Batch: int; WindowSamples: int; OverlapSamples: int }
type ISymbolicInference = abstract Infer : float32 array * int64 -> SymbolicEvent list

type NullInference () =
    interface ISymbolicInference with
        member _.Infer (_, position) = [{ SamplePosition = position; Label = "silence" }]

/// Coordinates independent, overlapped windows without imposing a model dependency.
type MuScriptorCoordinator (analysis: PcmRingBuffer, inference: ISymbolicInference, options: InferenceOptions) =
    let queue = ConcurrentQueue<SymbolicEvent>()
    let block = Array.zeroCreate<float32> 2048
    let mutable running = false
    let mutable worker: Thread option = None
    let gate = obj ()
    let buffer = Array.zeroCreate<float32> options.WindowSamples
    let loop () =
        let mutable position = 0L
        let mutable filled = 0
        let mutable expected = 0L
        let mutable count = 0
        let mutable blockPosition = 0L
        while Volatile.Read &running do
            if analysis.TryRead(block, &count, &blockPosition) then
                if filled > 0 && blockPosition <> expected then filled <- 0
                let mutable sourceOffset = 0
                while sourceOffset < count do
                    if filled = 0 then position <- blockPosition + int64 sourceOffset
                    let take = min (count - sourceOffset) (options.WindowSamples - filled)
                    Array.Copy(block, sourceOffset, buffer, filled, take)
                    sourceOffset <- sourceOffset + take
                    filled <- filled + take
                    expected <- blockPosition + int64 sourceOffset
                    if filled = options.WindowSamples then
                        for event in inference.Infer(buffer, position) do
                            if queue.Count < 1024 then queue.Enqueue event
                        let keep = min options.OverlapSamples options.WindowSamples
                        Array.Copy(buffer, options.WindowSamples - keep, buffer, 0, keep)
                        filled <- keep
                        position <- position + int64 (options.WindowSamples - keep)
                        expected <- blockPosition + int64 sourceOffset
            else Thread.Sleep 4
    member _.Start () = lock gate (fun () ->
        if not (Volatile.Read &running) then
            Volatile.Write(&running, true)
            let t = Thread(ThreadStart loop, IsBackground = true)
            worker <- Some t
            t.Start())
    member _.Stop () = lock gate (fun () ->
        Volatile.Write(&running, false)
        match worker with Some t when t.IsAlive -> t.Join() | _ -> ()
        worker <- None)
    member _.TryDequeue () = match queue.TryDequeue() with true, e -> Some e | _ -> None

type Obstacle = { SamplePosition: int64; Label: string }
type RenderSnapshot = { Obstacles: Obstacle array; SourceSampleClock: int64 }

type SimulationWorker (coordinator: MuScriptorCoordinator, clock: unit -> int64) =
    let mutable running = false
    let mutable snapshot = { Obstacles = [||]; SourceSampleClock = 0L }
    let mutable worker: Thread option = None
    let gate = obj ()
    let loop () =
        let obstacles = ResizeArray<Obstacle>()
        while Volatile.Read &running do
            match coordinator.TryDequeue() with
            | Some e ->
                obstacles.Add { SamplePosition = e.SamplePosition; Label = e.Label }
                Volatile.Write(&snapshot, { Obstacles = obstacles.ToArray(); SourceSampleClock = clock() })
            | None -> Thread.Sleep 8
    member _.Snapshot = Volatile.Read &snapshot
    member _.Start () = lock gate (fun () ->
        if not (Volatile.Read &running) then
            Volatile.Write(&running, true)
            let t = Thread(ThreadStart loop, IsBackground = true)
            worker <- Some t
            t.Start())
    member _.Stop () = lock gate (fun () ->
        Volatile.Write(&running, false)
        match worker with Some t when t.IsAlive -> t.Join() | _ -> ()
        worker <- None)

type CompositionRoot (?browser: IBrowserBridge) =
    static let mutable shared : CompositionRoot option = None
    let analysis = PcmRingBuffer(64, 2048)
    // 384 blocks x 2048 samples ~= 16 seconds of mono 48 kHz playback.
    let playback = PcmRingBuffer(384, 2048)
    let ingress = AudioIngress(analysis, playback)
    let coordinator = MuScriptorCoordinator(analysis, NullInference(), { PreludeForcing = false; Beam = 1; Batch = 1; WindowSamples = 240000; OverlapSamples = 14400 })
    let simulation = SimulationWorker(coordinator, fun () -> ingress.SourceSampleClock)
    let bridge = defaultArg browser (NullBrowserBridge() :> IBrowserBridge)
    let browserEvents = ConcurrentQueue<BrowserEvent>()
    let mutable latestBrowserEvent : BrowserEvent option = None
    let browserEventGate = obj ()
    let lifecycleGate = obj ()
    let mutable started = false
    let mutable subscription : IDisposable option = None
    static member Current = shared
    member _.Ingress = ingress
    member _.Simulation = simulation
    member _.LatestBrowserEvent = lock browserEventGate (fun () -> latestBrowserEvent)
    member _.TryReadPlayback (destination: float32 array, count: byref<int>, position: byref<int64>) =
        ingress.TryReadPlayback(destination, &count, &position)
    member _.TryDequeueBrowserEvent () = match browserEvents.TryDequeue() with true, event -> Some event | _ -> None
    member _.Pump () =
        match bridge.TryReadAudio() with
        | Some (samples, offset, count) -> ingress.Submit(samples, offset, count)
        | None -> ()
    member this.Start () = lock lifecycleGate (fun () ->
        if not started then
            subscription <- Some ((bridge.Events :> IObservable<BrowserEvent>).Subscribe(fun event ->
                lock browserEventGate (fun () -> latestBrowserEvent <- Some event)
                if browserEvents.Count < 1024 then browserEvents.Enqueue event))
            bridge.Start(); coordinator.Start(); simulation.Start()
            started <- true
            shared <- Some this)
    member this.Stop () = lock lifecycleGate (fun () ->
        if started then
            simulation.Stop(); coordinator.Stop(); bridge.Stop()
            subscription |> Option.iter (fun disposable -> disposable.Dispose())
            subscription <- None
            started <- false
            if shared |> Option.exists (fun current -> obj.ReferenceEquals(current, this)) then shared <- None)
