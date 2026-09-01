namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

/// Physical representations compared by the M1 experiment.
type M1BodyCandidate =
    | LegacyGraph
    | SimplifiedRing
    | StableHull

/// Direct-manipulation mappings compared against the same bodies and rooms.
type M1ControlMode =
    | GrabThrow
    | PullSling
    | SwipeSmack

/// Controlled spaces used for free play and landing measurement.
type M1Room =
    | EmptyToyRoom
    | GenerousTargetRoom

/// Input provenance stays explicit so touch can use the same control path as mouse and replay.
type M1PointerDevice =
    | MousePointer
    | TouchPointer of int64
    | ReplayPointer

type M1PointerPhase =
    | PointerIdle
    | PointerPressed
    | PointerHeld
    | PointerReleased

type M1PointerSample =
    { Tick : int
      Position : Vector2
      Phase : M1PointerPhase
      Device : M1PointerDevice }

type M1ControlConfiguration =
    { FixedDeltaSeconds : single
      GrabRadius : single
      GrabSpring : single
      GrabDamping : single
      PullHoldSpring : single
      PullHoldDamping : single
      GrabReleaseGain : single
      PullImpulsePerPixel : single
      SwipeImpulseGain : single
      MaximumForce : single
      MaximumImpulse : single
      MaximumSpeed : single }

[<RequireQualifiedAccess>]
module M1ControlConfiguration =

    let defaultConfiguration =
        { FixedDeltaSeconds = 1.0f / 60.0f
          GrabRadius = 72.0f
          GrabSpring = 34.0f
          GrabDamping = 7.5f
          PullHoldSpring = 8.0f
          PullHoldDamping = 4.0f
          GrabReleaseGain = 0.16f
          PullImpulsePerPixel = 2.8f
          SwipeImpulseGain = 0.2f
          MaximumForce = 2400.0f
          MaximumImpulse = 420.0f
          MaximumSpeed = 620.0f }

type M1ActiveControl =
    { PressPosition : Vector2
      BodyPositionAtPress : Vector2
      GrabOffset : Vector2
      PreviousPosition : Vector2
      PreviousTick : int
      PointerVelocity : Vector2 }

type M1ControlState =
    | ControlInactive
    | ControlActive of M1ActiveControl

type M1ControlOutput =
    { State : M1ControlState
      Force : Vector2
      Impulse : Vector2
      Started : bool
      Released : bool }

[<RequireQualifiedAccess>]
module M1Control =

    let clampMagnitude maximum (value : Vector2) =
        let magnitudeSquared = value.LengthSquared ()
        if magnitudeSquared > maximum * maximum && magnitudeSquared > 0.0f then
            value * (maximum / sqrt magnitudeSquared)
        else value

    let clampSpeed (configuration : M1ControlConfiguration) (velocity : Vector2) =
        clampMagnitude configuration.MaximumSpeed velocity

    /// Calculate the bounded release impulse used by both control execution and live previews.
    let releaseImpulse
        (configuration : M1ControlConfiguration)
        (mode : M1ControlMode)
        (pressPosition : Vector2)
        (pointerPosition : Vector2)
        (pointerVelocity : Vector2) =
        (match mode with
         | GrabThrow -> pointerVelocity * configuration.GrabReleaseGain
         | PullSling -> (pressPosition - pointerPosition) * configuration.PullImpulsePerPixel
         | SwipeSmack -> pointerVelocity * configuration.SwipeImpulseGain)
        |> clampMagnitude configuration.MaximumImpulse

    let private pointerVelocity (configuration : M1ControlConfiguration) (sample : M1PointerSample) (active : M1ActiveControl) =
        let elapsedTicks = max 1 (sample.Tick - active.PreviousTick)
        let elapsed = single elapsedTicks * configuration.FixedDeltaSeconds
        (sample.Position - active.PreviousPosition) / elapsed

    let private continueControl
        (configuration : M1ControlConfiguration)
        (mode : M1ControlMode)
        (bodyPosition : Vector2)
        (bodyVelocity : Vector2)
        (sample : M1PointerSample)
        (active : M1ActiveControl) =
        let velocity = pointerVelocity configuration sample active
        let active =
            { active with
                PreviousPosition = sample.Position
                PreviousTick = sample.Tick
                PointerVelocity = velocity }
        let force =
            (match mode with
             | GrabThrow ->
                 let target = sample.Position - active.GrabOffset
                 (target - bodyPosition) * configuration.GrabSpring - bodyVelocity * configuration.GrabDamping
             | PullSling ->
                 (active.BodyPositionAtPress - bodyPosition) * configuration.PullHoldSpring -
                 bodyVelocity * configuration.PullHoldDamping
             | SwipeSmack -> v2Zero)
            |> clampMagnitude configuration.MaximumForce
        { State = ControlActive active
          Force = force
          Impulse = v2Zero
          Started = false
          Released = false }

    let private releaseControl
        (configuration : M1ControlConfiguration)
        (mode : M1ControlMode)
        (sample : M1PointerSample)
        (active : M1ActiveControl) =
        let measuredVelocity = pointerVelocity configuration sample active
        let velocity = if measuredVelocity.LengthSquared () > 1.0f then measuredVelocity else active.PointerVelocity
        let impulse = releaseImpulse configuration mode active.PressPosition sample.Position velocity
        { State = ControlInactive
          Force = v2Zero
          Impulse = impulse
          Started = false
          Released = true }

    /// Advance one control sample without mutating physics. The caller applies the bounded result.
    let step
        (configuration : M1ControlConfiguration)
        (mode : M1ControlMode)
        (bodyPosition : Vector2)
        (bodyVelocity : Vector2)
        (sample : M1PointerSample)
        (state : M1ControlState) =
        match sample.Phase, state with
        | PointerPressed, ControlInactive when Vector2.Distance (sample.Position, bodyPosition) <= configuration.GrabRadius ->
            let active =
                { PressPosition = sample.Position
                  BodyPositionAtPress = bodyPosition
                  GrabOffset = sample.Position - bodyPosition
                  PreviousPosition = sample.Position
                  PreviousTick = sample.Tick
                  PointerVelocity = v2Zero }
            { State = ControlActive active
              Force = v2Zero
              Impulse = v2Zero
              Started = true
              Released = false }
        | (PointerPressed | PointerHeld), ControlActive active ->
            continueControl configuration mode bodyPosition bodyVelocity sample active
        | PointerReleased, ControlActive active ->
            releaseControl configuration mode sample active
        | PointerIdle, ControlActive active ->
            { State = ControlActive active
              Force = v2Zero
              Impulse = v2Zero
              Started = false
              Released = false }
        | _, _ ->
            { State = ControlInactive
              Force = v2Zero
              Impulse = v2Zero
              Started = false
              Released = false }

    /// Preserve control state while the world is paused; callers may still render the current sample.
    let stepWhenAdvancing advancing configuration mode bodyPosition bodyVelocity sample state =
        if advancing then
            step configuration mode bodyPosition bodyVelocity sample state
        else
            { State = state
              Force = v2Zero
              Impulse = v2Zero
              Started = false
              Released = false }

[<RequireQualifiedAccess>]
module M1Trace =

    let normalize samples =
        samples
        |> Seq.mapi (fun tick sample -> { sample with Tick = tick })
        |> Seq.toArray

    let private phaseCode = function
        | PointerIdle -> 0u
        | PointerPressed -> 1u
        | PointerHeld -> 2u
        | PointerReleased -> 3u

    let private deviceCode = function
        | MousePointer -> 1UL
        | TouchPointer pointerId -> 2UL ^^^ uint64 pointerId
        | ReplayPointer -> 3UL

    let checksum samples =
        let mix hash value = (hash ^^^ value) * 1099511628211UL
        let mutable hash = 14695981039346656037UL
        for sample in samples do
            hash <- mix hash (uint64 sample.Tick)
            hash <- mix hash (uint64 (BitConverter.SingleToInt32Bits sample.Position.X))
            hash <- mix hash (uint64 (BitConverter.SingleToInt32Bits sample.Position.Y))
            hash <- mix hash (uint64 (phaseCode sample.Phase))
            hash <- mix hash (deviceCode sample.Device)
        sprintf "%016X" hash

    let asReplay samples =
        samples |> Array.map (fun sample -> { sample with Device = ReplayPointer })

[<RequireQualifiedAccess>]
module M1Topology =

    let bodyCount = function
        | LegacyGraph -> 33
        | SimplifiedRing -> 13
        | StableHull -> 1

    let jointCount = function
        | LegacyGraph -> 528
        | SimplifiedRing -> 24
        | StableHull -> 0

    let constraintReduction candidate =
        1.0f - single (jointCount candidate) / single (jointCount LegacyGraph)

type M1VerificationResult =
    { Passed : bool
      Checks : string array }

[<RequireQualifiedAccess>]
module M1Verification =

    let private samples =
        [|{ Tick = 0; Position = v2 0.0f 0.0f; Phase = PointerPressed; Device = MousePointer }
          { Tick = 1; Position = v2 24.0f 10.0f; Phase = PointerHeld; Device = MousePointer }
          { Tick = 2; Position = v2 58.0f 26.0f; Phase = PointerHeld; Device = MousePointer }
          { Tick = 3; Position = v2 96.0f 44.0f; Phase = PointerReleased; Device = MousePointer }|]

    let private simulate mode replaySamples =
        let configuration = M1ControlConfiguration.defaultConfiguration
        let mutable state = ControlInactive
        let mutable position = v2Zero
        let mutable velocity = v2Zero
        let mutable maximumForce = 0.0f
        let mutable maximumImpulse = 0.0f
        for sample in replaySamples do
            let output = M1Control.step configuration mode position velocity sample state
            state <- output.State
            maximumForce <- max maximumForce (output.Force.Length ())
            maximumImpulse <- max maximumImpulse (output.Impulse.Length ())
            velocity <- velocity + output.Force * configuration.FixedDeltaSeconds * 0.02f + output.Impulse
            velocity <- M1Control.clampSpeed configuration velocity
            position <- position + velocity * configuration.FixedDeltaSeconds
        (position, velocity, maximumForce, maximumImpulse)

    let evaluate () =
        let configuration = M1ControlConfiguration.defaultConfiguration
        let trace = M1Trace.normalize samples
        let replay = M1Trace.asReplay trace
        let checks = ResizeArray<string> ()
        let mutable passed = true
        let check condition description =
            passed <- passed && condition
            checks.Add ((if condition then "PASS " else "FAIL ") + description)
        check (M1Topology.bodyCount SimplifiedRing < M1Topology.bodyCount LegacyGraph)
            "simplified ring reduces body count"
        check (M1Topology.jointCount SimplifiedRing <= 24)
            "simplified ring reduces the legacy 528-joint graph to 24 joints"
        check (M1Topology.jointCount StableHull = 0)
            "stable hull has no constraints"
        check (M1Trace.checksum trace = M1Trace.checksum (M1Trace.normalize trace))
            "trace normalization and checksum are deterministic"
        for mode in [GrabThrow; PullSling; SwipeSmack] do
            let first = simulate mode replay
            let second = simulate mode replay
            let (position, velocity, maximumForce, maximumImpulse) = first
            let (position2, velocity2, _, _) = second
            check (Vector2.Distance (position, position2) < 0.0001f && Vector2.Distance (velocity, velocity2) < 0.0001f)
                (sprintf "%A replay is deterministic" mode)
            check (maximumForce <= configuration.MaximumForce + 0.001f)
                (sprintf "%A force is bounded" mode)
            check (maximumImpulse <= configuration.MaximumImpulse + 0.001f)
                (sprintf "%A impulse is bounded" mode)
            check (velocity.Length () <= configuration.MaximumSpeed + 0.001f)
                (sprintf "%A speed is bounded" mode)
        { Passed = passed; Checks = checks.ToArray () }

    let report () =
        let result = evaluate ()
        for check in result.Checks do Console.WriteLine check
        Console.WriteLine (sprintf "M1 trace %s" (M1Trace.checksum (M1Trace.normalize samples)))
        if result.Passed then 0 else 1

[<RequireQualifiedAccess>]
module M1Launch =
    let mutable Direct = false