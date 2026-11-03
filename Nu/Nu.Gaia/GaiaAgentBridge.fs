// Gaia - The Nu Game Engine editor.
// Required Notice:
// Copyright (C) Bryan Edds.
// Gaia - The Nu Game Engine editor is licensed under the Nu Game Engine Noncommercial License.
// See: https://github.com/bryanedds/Nu/master/License.md

// EXPERIMENTAL (agentic-gaia spike): a local, opt-in request bridge that lets an external agent drive Gaia.
// Transport only: requests are read on background threads and queued; Gaia drains the queue on the main
// (editor) thread once per frame, so no world or FSI access ever happens off the main thread.

namespace Nu.Gaia
open System
open System.Collections.Concurrent
open System.IO
open System.Net.Sockets
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Prime
open Nu

/// A compiler diagnostic from an interactive evaluation, flattened for JSON.
type EvalDiagnostic =
    { Severity : string
      Code : string
      File : string
      StartLine : int
      StartColumn : int
      EndLine : int
      EndColumn : int
      Message : string }

    static member make (diag : FSharp.Compiler.Diagnostics.FSharpDiagnostic) =
        { Severity = string diag.Severity
          Code = diag.ErrorNumberText
          File = diag.FileName
          StartLine = diag.StartLine
          StartColumn = diag.StartColumn
          EndLine = diag.EndLine
          EndColumn = diag.EndColumn
          Message = diag.Message }

/// The structured outcome of one interactive evaluation.
type EvalOutcome =
    { Kind : string // "ok" | "compile_error" | "runtime_error"
      Output : string
      Warnings : string
      Exception : string
      Diagnostics : EvalDiagnostic array }

    static member empty =
        { Kind = "none"; Output = ""; Warnings = ""; Exception = ""; Diagnostics = [||] }

/// A request from an external agent, completed on the editor thread.
/// State is 0 = queued, 1 = claimed by the editor thread, 2 = withdrawn by the transport after a client timeout.
type AgentRequest =
    { Id : string
      Op : string
      Args : JsonElement
      Reply : TaskCompletionSource<string>
      State : int ref }

[<RequireQualifiedAccess>]
module AgentBridge =

    /// Environment variable naming the Unix domain socket path. The bridge is off unless it is set.
    let [<Literal>] SocketEnvVar = "NU_GAIA_AGENT_SOCKET"

    /// Default upper bound on how long a connection waits for the editor thread to answer one request.
    /// A request may lower or raise it with a top-level "timeout_s" (1..600).
    let ReplyTimeout = TimeSpan.FromSeconds 120.0

    /// If the editor thread has produced no frame for this long, new requests fail fast instead of queueing.
    /// (A legitimately long op such as reload_code also trips this; the error names the op that is running.)
    let UnresponsiveThreshold = TimeSpan.FromSeconds 10.0

    /// Requests beyond this many in flight (queued, running or awaiting a deferred reply) are refused.
    let MaxInFlight = 64

    /// Connections beyond this many open at once are closed immediately.
    let MaxConnections = 16

    /// Requests waiting for the editor thread.
    let Requests = ConcurrentQueue<AgentRequest> ()

    let mutable private ListenerOpt = Option<Socket * string>.None
    let private InFlight = ref 0
    let private Connections = ref 0

    // liveness, written by the editor thread and read by connection threads (all under Gate)
    let private Gate = obj ()
    let private Clock = Diagnostics.Stopwatch.StartNew ()
    let mutable private LastFrame = 0L
    let mutable private LastFrameMs = 0L
    let mutable private BusyId = ""
    let mutable private BusyOp = ""
    let mutable private BusySinceMs = 0L
    let mutable private PendingException = ""

    /// Whether the bridge is listening. When false, Gaia skips all bridge bookkeeping.
    let mutable Active = false

    /// Record that the editor thread completed a frame, and any pending unhandled-exception dialog. Editor thread only.
    let heartbeat (frame : int64) (pendingException : string) =
        lock Gate (fun () -> LastFrame <- frame; LastFrameMs <- Clock.ElapsedMilliseconds; PendingException <- pendingException)

    /// Atomically claim a queued request for execution; false if the transport already withdrew it. Editor thread only.
    let tryClaim (request : AgentRequest) =
        Interlocked.CompareExchange (&request.State.contents, 1, 0) = 0

    /// Mark a request as running on the editor thread. Editor thread only.
    let beginRequest (request : AgentRequest) =
        lock Gate (fun () -> BusyId <- request.Id; BusyOp <- request.Op; BusySinceMs <- Clock.ElapsedMilliseconds)

    /// Mark the editor thread idle again. Editor thread only.
    let endRequest () =
        lock Gate (fun () -> BusyId <- ""; BusyOp <- "")

    /// Liveness as seen from a connection thread, without touching the editor thread.
    let private liveness () =
        lock Gate (fun () ->
            let now = Clock.ElapsedMilliseconds
            let sinceFrameMs = if LastFrame = 0L then 0L else now - LastFrameMs
            let busyMs = if BusyOp = "" then 0L else now - BusySinceMs
            {| Frame = LastFrame
               SinceFrameMs = sinceFrameMs
               BusyId = BusyId
               BusyOp = BusyOp
               BusyMs = busyMs
               PendingException = PendingException
               Unresponsive = LastFrame > 0L && float sinceFrameMs > UnresponsiveThreshold.TotalMilliseconds |})

    /// Serialize a reply object to a single JSON line.
    let toJson (value : obj) =
        JsonSerializer.Serialize value

    /// Make an error reply.
    let error (id : string) (message : string) =
        toJson (dict ["id", box id; "ok", box false; "error", box message])

    let private livenessFields (id : string) ok =
        let live = liveness ()
        let fields = Collections.Generic.Dictionary<string, obj> ()
        fields["id"] <- box id
        fields["ok"] <- box ok
        fields["frame"] <- box live.Frame
        fields["msSinceFrame"] <- box live.SinceFrameMs
        fields["busyOp"] <- box live.BusyOp
        fields["busyId"] <- box live.BusyId
        fields["busyMs"] <- box live.BusyMs
        fields["queued"] <- box Requests.Count
        fields["inFlight"] <- box (Volatile.Read &InFlight.contents)
        fields["unresponsive"] <- box live.Unresponsive
        fields["pendingException"] <- box (if live.PendingException = "" then null else live.PendingException)
        fields

    let private unresponsiveError (id : string) =
        let live = liveness ()
        let fields = livenessFields id false
        fields["error"] <-
            "Editor thread unresponsive for " + string live.SinceFrameMs + " ms" +
            (if live.BusyOp <> "" then " (running op '" + live.BusyOp + "' id " + live.BusyId + " for " + string live.BusyMs + " ms)" else "") +
            "; request NOT queued. A non-terminating eval cannot be cancelled in-process: poll ping, or restart Gaia."
        toJson fields

    let private parseRequest (line : string) =
        use doc = JsonDocument.Parse line
        let root = doc.RootElement
        let id = match root.TryGetProperty "id" with | (true, v) -> v.ToString () | _ -> ""
        let op = match root.TryGetProperty "op" with | (true, v) when v.ValueKind = JsonValueKind.String -> v.GetString () | _ -> ""
        let timeout =
            match root.TryGetProperty "timeout_s" with
            | (true, v) when v.ValueKind = JsonValueKind.Number -> TimeSpan.FromSeconds (Math.Clamp (v.GetDouble (), 1.0, 600.0))
            | _ -> ReplyTimeout
        let args = match root.TryGetProperty "args" with | (true, v) -> v.Clone () | _ -> JsonDocument.Parse("{}").RootElement.Clone ()
        (id, op, timeout, args)

    let private handleRequest (line : string) : Task<string> =
        task {
            let (id, op, timeout, args) = parseRequest line
            if op = "ping" then
                // answered here, never queued: works while the editor thread is blocked
                return toJson (livenessFields id true)
            elif (liveness ()).Unresponsive then return unresponsiveError id
            elif Interlocked.Increment &InFlight.contents > MaxInFlight then
                Interlocked.Decrement &InFlight.contents |> ignore<int>
                return error id ("Too many requests in flight (" + string MaxInFlight + "); request not queued.")
            else
                try let tcs = TaskCompletionSource<string> TaskCreationOptions.RunContinuationsAsynchronously
                    let request = { Id = id; Op = op; Args = args; Reply = tcs; State = ref 0 }
                    Requests.Enqueue request
                    let! winner = Task.WhenAny (tcs.Task :> Task, Task.Delay timeout)
                    if obj.ReferenceEquals (winner, tcs.Task) then return tcs.Task.Result
                    else
                        // withdraw it so it can never run after the client gave up; if it already started, say so
                        let withdrawn = Interlocked.CompareExchange (&request.State.contents, 2, 0) = 0
                        tcs.TrySetCanceled () |> ignore<bool>
                        if tcs.Task.IsCompletedSuccessfully then return tcs.Task.Result // finished in the race window
                        else
                            let fields = livenessFields id false
                            fields["timedOut"] <- box true
                            fields["withdrawn"] <- box withdrawn
                            fields["error"] <-
                                "Timed out after " + string timeout.TotalSeconds + " s. " +
                                (if withdrawn then "The request had not started and was withdrawn; it will not run."
                                 else "The request had already started on the editor thread and cannot be cancelled; its effects may still apply when it finishes (check status/undo later).")
                            return toJson fields
                finally Interlocked.Decrement &InFlight.contents |> ignore<int>
        }

    let private handleConnection (client : Socket) : Task =
        task {
            try use stream = new NetworkStream (client, true)
                use reader = new StreamReader (stream, UTF8Encoding false)
                use writer = new StreamWriter (stream, UTF8Encoding false)
                writer.AutoFlush <- true
                let mutable running = true
                while running do
                    let! line = reader.ReadLineAsync ()
                    if isNull line then running <- false
                    elif not (String.IsNullOrWhiteSpace line) then
                        let! reply =
                            task {
                                try return! handleRequest line
                                with exn -> return error "" ("Bad request: " + exn.Message)
                            }
                        try do! writer.WriteLineAsync reply
                        with _ -> running <- false // client went away
            finally Interlocked.Decrement &Connections.contents |> ignore<int>
        }

    let private isPrivateDirectory (dir : string) =
        let info = DirectoryInfo dir
        let groupOrOther =
            UnixFileMode.GroupRead ||| UnixFileMode.GroupWrite ||| UnixFileMode.GroupExecute |||
            UnixFileMode.OtherRead ||| UnixFileMode.OtherWrite ||| UnixFileMode.OtherExecute
        info.Exists && isNull info.LinkTarget && (File.GetUnixFileMode dir &&& groupOrOther) = enum 0

    let private isLive (socketPath : string) =
        try use probe = new Socket (AddressFamily.Unix, SocketType.Stream, ProtocolType.Unspecified)
            probe.Connect (UnixDomainSocketEndPoint socketPath)
            true
        with _ -> false

    /// Stop listening and remove the socket file. Idempotent.
    let stop () =
        match ListenerOpt with
        | Some (listener, socketPath) ->
            ListenerOpt <- None
            Active <- false
            try listener.Dispose () with _ -> ()
            try File.Delete socketPath with _ -> ()
        | None -> ()

    /// Start listening if the socket environment variable is set. Idempotent.
    let tryStart () =
        match (Environment.GetEnvironmentVariable SocketEnvVar, ListenerOpt) with
        | (null, _) | ("", _) | (_, Some _) -> ()
        | (socketPath, None) ->
            let socketPath = Path.GetFullPath socketPath
            let dir = Path.GetDirectoryName socketPath
            // the socket is arbitrary code execution as this user, so it must live in a private (0700, non-symlink)
            // directory such as $XDG_RUNTIME_DIR; that also closes the window between bind and chmod below
            if not (OperatingSystem.IsWindows ()) && not (isPrivateDirectory dir) then
                Log.error ("Agent bridge not started: " + dir + " must be a private directory (mode 0700, not a symlink).")
            elif isLive socketPath then
                Log.error ("Agent bridge not started: another process is already listening on " + socketPath + ".")
            else
                let listener = new Socket (AddressFamily.Unix, SocketType.Stream, ProtocolType.Unspecified)
                try if File.Exists socketPath then File.Delete socketPath // stale socket from a dead instance
                    listener.Bind (UnixDomainSocketEndPoint socketPath)
                    if not (OperatingSystem.IsWindows ()) then
                        File.SetUnixFileMode (socketPath, UnixFileMode.UserRead ||| UnixFileMode.UserWrite)
                    listener.Listen 16
                    ListenerOpt <- Some (listener, socketPath)
                    Active <- true
                    AppDomain.CurrentDomain.ProcessExit.Add (fun _ -> stop ())
                    let acceptLoop () =
                        let mutable listening = true
                        while listening do
                            try let client = listener.Accept ()
                                if Interlocked.Increment &Connections.contents > MaxConnections then
                                    Interlocked.Decrement &Connections.contents |> ignore<int>
                                    client.Dispose ()
                                else Task.Run<unit> (fun () -> task { do! handleConnection client }) |> ignore<Task<unit>>
                            with
                            | :? ObjectDisposedException -> listening <- false
                            | exn ->
                                // never let an accept error kill the editor process; back off briefly
                                Console.Error.WriteLine ("Agent bridge accept error: " + exn.Message)
                                Thread.Sleep 100
                    let thread = Thread (acceptLoop, IsBackground = true, Name = "GaiaAgentBridge")
                    thread.Start ()
                    Log.info ("Agent bridge listening on " + socketPath)
                with exn ->
                    listener.Dispose ()
                    Log.error ("Agent bridge failed to start: " + exn.Message)
