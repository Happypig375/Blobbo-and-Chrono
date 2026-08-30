namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu
open BlobboPlayground

type M1LandingOutcome =
    | AwaitingAttempt
    | FreePlayComplete
    | TargetInFlight
    | TargetHit
    | TargetMiss

type M1SceneState =
    { Candidate : M1BodyCandidate
      ControlMode : M1ControlMode
      Room : M1Room
      ControlState : M1ControlState
      CaptureRev : M1PointerSample list
      LastTrace : M1PointerSample array
      ReplayIndexOpt : int option
      Attempts : int
      Hits : int
      Misses : int
      Outcome : M1LandingOutcome
      FlightSeconds : single
      StableSeconds : single
      FixtureVersion : int
      ResetCount : int
      ReplayCount : int
      Trail : Vector2 array
      LastForce : Vector2
      LastImpulse : Vector2 }

[<RequireQualifiedAccess>]
module M1SceneState =

    let initial =
        { Candidate = SimplifiedRing
          ControlMode = GrabThrow
          Room = EmptyToyRoom
          ControlState = ControlInactive
          CaptureRev = []
          LastTrace = Array.empty
          ReplayIndexOpt = None
          Attempts = 0
          Hits = 0
          Misses = 0
          Outcome = AwaitingAttempt
          FlightSeconds = 0.0f
          StableSeconds = 0.0f
          FixtureVersion = 0
          ResetCount = 0
          ReplayCount = 0
          Trail = Array.empty
          LastForce = v2Zero
          LastImpulse = v2Zero }

    let rebuild candidate controlMode room state =
        { initial with
            Candidate = candidate
            ControlMode = controlMode
            Room = room
            LastTrace = state.LastTrace
            FixtureVersion = state.FixtureVersion + 1
            ResetCount = state.ResetCount
            ReplayCount = state.ReplayCount }

    let reset state =
        { state with
            ControlState = ControlInactive
            CaptureRev = []
            ReplayIndexOpt = None
            Outcome = AwaitingAttempt
            FlightSeconds = 0.0f
            StableSeconds = 0.0f
            FixtureVersion = state.FixtureVersion + 1
            ResetCount = state.ResetCount + 1
            Trail = Array.empty
            LastForce = v2Zero
            LastImpulse = v2Zero }

[<RequireQualifiedAccess>]
module M1PointerInput =

    let fromMouse tick position world =
        let phase =
            if World.isMouseButtonPressed MouseLeft world then PointerPressed
            elif World.isMouseButtonReleased MouseLeft world then PointerReleased
            elif World.isMouseButtonDown MouseLeft world then PointerHeld
            else PointerIdle
        { Tick = tick
          Position = position
          Phase = phase
          Device = MousePointer }

    /// Touch platforms feed this adapter without changing control or replay semantics.
    let fromTouch tick pointerId position phase =
        { Tick = tick
          Position = position
          Phase = phase
          Device = TouchPointer pointerId }

module [<AutoOpen>] M1SceneExtensions =
    type Screen with
        member this.GetM1SceneState world : M1SceneState = this.Get (nameof this.M1SceneState) world
        member this.SetM1SceneState (value : M1SceneState) world = this.Set (nameof this.M1SceneState) value world
        member this.M1SceneState = lens (nameof this.M1SceneState) this this.GetM1SceneState this.SetM1SceneState

[<RequireQualifiedAccess>]
module M1SceneVisual =

    let sprite name (position : Vector2) (size : Vector2) (elevation : single) colorValue image world =
        World.doStaticSprite name
            [Entity.Position .= position.V3
             Entity.Size .= size.V3
             Entity.Elevation .= elevation
             Entity.StaticImage .= image
             Entity.Color .= colorValue]
            world
        |> ignore

    let segment name (startPoint : Vector2) (stopPoint : Vector2) (thickness : single) (elevation : single) colorValue world =
        let delta = stopPoint - startPoint
        let length = delta.Length ()
        if length > 0.001f then
            let midpoint = (startPoint + stopPoint) * 0.5f
            let angle = atan2 delta.Y delta.X
            World.doStaticSprite name
                [Entity.Position .= midpoint.V3
                 Entity.Size .= v3 length thickness 0.0f
                 Entity.Rotation .= Quaternion.CreateFromAxisAngle (Vector3.UnitZ, angle)
                 Entity.Elevation .= elevation
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= colorValue]
                world
            |> ignore

    let button name text (position : Vector2) (width : single) world =
        World.doButton name
            [Entity.Position .= position.V3
             Entity.Size .= v3 width 24.0f 0.0f
             Entity.Elevation .= 30.0f
             Entity.FontSizing .= Some 8.0f
             Entity.Text @= text]
            world

type Scene06_M1ControlStudyDispatcher () =
    inherit ScreenDispatcherImSim ()

    static let subjectSpawn = v2 -175.0f -15.0f
    static let playBounds = box2 (v2 -310.0f -82.0f) (v2 620.0f 150.0f)
    static let targetBounds = box2 (v2 105.0f -67.0f) (v2 170.0f 120.0f)

    static member Properties =
        [define Screen.GameplayState Quit
         define Screen.M1SceneState M1SceneState.initial]

    static member private CenterSnapshot candidate (subject : Entity) world =
        match candidate with
        | LegacyGraph -> subject.GetBlobboCenter world
        | SimplifiedRing
        | StableHull -> subject.GetM1BodyCenter world

    static member private OutcomeText = function
        | AwaitingAttempt -> "READY"
        | FreePlayComplete -> "FREE PLAY"
        | TargetInFlight -> "IN FLIGHT"
        | TargetHit -> "TARGET HIT"
        | TargetMiss -> "MISS - TRY AGAIN"

    static member private CandidateText = function
        | LegacyGraph -> "LEGACY 33 / 528"
        | SimplifiedRing -> "RING 13 / 24"
        | StableHull -> "HULL 1 / 0"

    static member private ControlText = function
        | GrabThrow -> "GRAB + THROW"
        | PullSling -> "PULL + SLING"
        | SwipeSmack -> "SWIPE + SMACK"

    override _.Process (_, screen, world) =
        if screen.GetSelected world then
            let configuration = M1ControlConfiguration.defaultConfiguration
            let mutable state = screen.GetM1SceneState world
            let fixtureName = sprintf "M1 Fixture %d" state.FixtureVersion
            World.beginGroup fixtureName [] world

            // Structured procedural backdrop: deterministic stripes, grid, and stars.
            M1SceneVisual.sprite "Backdrop" v2Zero (World.getDisplayVirtualResolution ()).V2 -30.0f (color 0.025f 0.035f 0.09f 1.0f) Assets.Default.White world
            for index in 0 .. 5 do
                let y = -150.0f + single index * 60.0f
                let tint = if index % 2 = 0 then color 0.05f 0.1f 0.2f 0.55f else color 0.08f 0.04f 0.17f 0.5f
                M1SceneVisual.sprite (sprintf "Band %d" index) (v2 0.0f y) (v2 640.0f 58.0f) -29.0f tint Assets.Default.White world
            for index in -5 .. 5 do
                M1SceneVisual.segment (sprintf "Grid V %d" index) (v2 (single index * 64.0f) (-180.0f)) (v2 (single index * 64.0f) 180.0f) 1.0f -27.0f (color 0.22f 0.35f 0.55f 0.12f) world
            for index in -3 .. 3 do
                M1SceneVisual.segment (sprintf "Grid H %d" index) (v2 (-320.0f) (single index * 52.0f)) (v2 320.0f (single index * 52.0f)) 1.0f -27.0f (color 0.22f 0.35f 0.55f 0.12f) world
            for index in 0 .. 25 do
                let x = single ((index * 83 + 29) % 610) - 305.0f
                let y = single ((index * 47 + 17) % 135) - 72.0f
                let diameter = 2.0f + single (index % 3) * 1.5f
                let starColor = if index % 4 = 0 then color 0.45f 0.85f 1.0f 0.8f else color 0.72f 0.66f 1.0f 0.5f
                M1SceneVisual.sprite (sprintf "Star %02d" index) (v2 x y) (v2Dup diameter) -25.0f starColor Assets.Default.Ball world

            World.doBlockBody2d "Room Border"
                [Entity.Position .= playBounds.Center.V3
                 Entity.Size .= playBounds.Size.V3
                 Entity.BodyShape .=
                    ContourShape
                        { Links =
                            [|v3 -0.5f 0.5f 0.0f
                              v3 0.5f 0.5f 0.0f
                              v3 0.5f -0.5f 0.0f
                              v3 -0.5f -0.5f 0.0f|]
                          Closed = true
                          TransformOpt = None
                          PropertiesOpt = None }
                 Entity.Visible .= false]
                world
            |> ignore

            if state.Room = GenerousTargetRoom then
                let targetCenter = targetBounds.Center
                M1SceneVisual.sprite "Target Outer" targetCenter (v2 138.0f 96.0f) -3.0f (color 0.18f 0.85f 1.0f 0.22f) Assets.Default.Ball world
                M1SceneVisual.sprite "Target Middle" targetCenter (v2 98.0f 70.0f) -2.9f (color 0.95f 0.24f 0.62f 0.5f) Assets.Default.Ball world
                M1SceneVisual.sprite "Target Core" targetCenter (v2 52.0f 38.0f) -2.8f (color 1.0f 0.88f 0.28f 0.85f) Assets.Default.Ball world
                World.doBlockBody2d "Target Deck"
                    [Entity.Position .= v3 targetCenter.X (targetBounds.Min.Y - 7.0f) 0.0f
                     Entity.Size .= v3 190.0f 14.0f 0.0f
                     Entity.BodyType .= Static
                     Entity.StaticImage .= Assets.Default.White
                     Entity.Color .= color 0.15f 0.65f 0.85f 0.8f]
                    world
                |> ignore

            match state.Candidate with
            | LegacyGraph ->
                World.doEntity<BlobboDispatcher> "Subject"
                    [Entity.Position .= subjectSpawn.V3
                     Entity.Size .= v3 96.0f 96.0f 0.0f
                     Entity.WorldFluidEmitter .= Address.empty]
                    world
            | SimplifiedRing
            | StableHull ->
                World.doEntity<M1BlobboDispatcher> "Subject"
                    [Entity.Position .= subjectSpawn.V3
                     Entity.Size .= v3 96.0f 96.0f 0.0f
                     Entity.M1BodyCandidate @= state.Candidate
                     Entity.M1FixtureVersion @= state.FixtureVersion]
                    world
            let subject = world.DeclaredEntity
            let centerSnapshot = Scene06_M1ControlStudyDispatcher.CenterSnapshot state.Candidate subject world
            let center =
                { centerSnapshot with
                    BodyCenter = (subject.GetPerimeter world).Center.V2 }
            let centerBodyId = { BodySource = subject; BodyIndex = M1BodyModel.CenterBodyIndex }

            let eyeBounds = World.getEye2dBounds world
            let rawPointer = World.getMousePosition2dWorld false world
            let mousePosition =
                v2
                    (max eyeBounds.Min.X (min eyeBounds.Max.X rawPointer.X))
                    (max eyeBounds.Min.Y (min eyeBounds.Max.Y rawPointer.Y))
            let replaying = state.ReplayIndexOpt.IsSome
            let advancing = world.TimeAdvancing
            let sample =
                match state.ReplayIndexOpt with
                | Some index when index < state.LastTrace.Length ->
                    { state.LastTrace[index] with Device = ReplayPointer }
                | _ ->
                    M1PointerInput.fromMouse (int (world.UpdateTime % int64 Int32.MaxValue)) mousePosition world

            let wasActive =
                match state.ControlState with
                | ControlActive _ -> true
                | ControlInactive -> false
            let output =
                M1Control.stepWhenAdvancing
                    advancing
                    configuration
                    state.ControlMode
                    center.BodyCenter
                    center.BodyLinearVelocity
                    sample
                    state.ControlState

            if advancing && World.getBodyExists centerBodyId world then
                if output.Force.LengthSquared () > 0.0f then
                    World.applyBodyForce output.Force.V3 None centerBodyId world
                if output.Impulse.LengthSquared () > 0.0f then
                    World.applyBodyLinearImpulse output.Impulse.V3 None centerBodyId world

            // Clamp every constituent body, not just the grabbed center.
            if advancing then
                for bodyId in M1BodyModel.bodyIds state.Candidate subject do
                    if World.getBodyExists bodyId world then
                        let velocity = World.getBodyLinearVelocity bodyId world
                        let clamped = M1Control.clampSpeed configuration velocity.V2
                        if Vector2.DistanceSquared (velocity.V2, clamped) > 0.001f then
                            World.setBodyLinearVelocity clamped.V3 bodyId world

            if state.Candidate <> LegacyGraph then
                let pull =
                    match output.State with
                    | ControlActive _ -> sample.Position - center.BodyCenter
                    | ControlInactive -> v2Zero
                subject.SetM1VisualPull pull world

            let mutable captureRev = state.CaptureRev
            let mutable lastTrace = state.LastTrace
            if advancing && not replaying then
                if output.Started then
                    captureRev <- [{ sample with Tick = 0 }]
                elif wasActive then
                    captureRev <- { sample with Tick = captureRev.Length } :: captureRev
                    if output.Released then
                        lastTrace <- captureRev |> List.rev |> M1Trace.normalize
                        captureRev <- []

            let replayIndexOpt =
                if advancing then
                    match state.ReplayIndexOpt with
                    | Some index when index + 1 < state.LastTrace.Length -> Some (index + 1)
                    | Some _ -> None
                    | None -> None
                else state.ReplayIndexOpt

            let mutable attempts = state.Attempts
            let mutable hits = state.Hits
            let mutable misses = state.Misses
            let mutable outcome = state.Outcome
            let mutable flightSeconds = state.FlightSeconds
            let mutable stableSeconds = state.StableSeconds
            if advancing && output.Released then
                attempts <- attempts + 1
                flightSeconds <- 0.0f
                stableSeconds <- 0.0f
                outcome <- if state.Room = GenerousTargetRoom then TargetInFlight else FreePlayComplete
            elif advancing && outcome = TargetInFlight then
                flightSeconds <- flightSeconds + configuration.FixedDeltaSeconds
                let speed = center.BodyLinearVelocity.Length ()
                stableSeconds <- if speed < 28.0f then stableSeconds + configuration.FixedDeltaSeconds else 0.0f
                let inTarget = targetBounds.Contains center.BodyCenter <> ContainmentType.Disjoint
                if inTarget && speed < 105.0f then
                    outcome <- TargetHit
                    hits <- hits + 1
                elif flightSeconds >= 5.0f || stableSeconds >= 0.55f then
                    outcome <- TargetMiss
                    misses <- misses + 1

            let trail =
                if wasActive || output.Started || outcome <> AwaitingAttempt || replaying then
                    let trail =
                        if state.Trail.Length = 0 || Vector2.DistanceSquared (Array.last state.Trail, center.BodyCenter) >= 36.0f then
                            Array.append state.Trail [|center.BodyCenter|]
                        else state.Trail
                    if trail.Length > 18 then trail[trail.Length - 18 ..] else trail
                else Array.empty

            if advancing then
                state <-
                    { state with
                        ControlState = output.State
                        CaptureRev = captureRev
                        LastTrace = lastTrace
                        ReplayIndexOpt = replayIndexOpt
                        Attempts = attempts
                        Hits = hits
                        Misses = misses
                        Outcome = outcome
                        FlightSeconds = flightSeconds
                        StableSeconds = stableSeconds
                        Trail = trail
                        LastForce = output.Force
                        LastImpulse = output.Impulse }

            // Motion trail, force tether, pointer halo, and pull trajectory.
            for index in 0 .. trail.Length - 1 do
                let progress = single (index + 1) / single trail.Length
                M1SceneVisual.sprite
                    (sprintf "Trail %02d" index)
                    trail[index]
                    (v2Dup (3.0f + progress * 8.0f))
                    2.0f
                    (color 0.3f 0.85f 1.0f (progress * 0.32f))
                    Assets.Default.Ball
                    world
            match output.State with
            | ControlActive active ->
                M1SceneVisual.segment "Control Tether" center.BodyCenter sample.Position 3.0f 4.0f (color 1.0f 0.82f 0.28f 0.8f) world
                M1SceneVisual.sprite "Pointer Halo" sample.Position (v2Dup 22.0f) 4.1f (color 1.0f 0.9f 0.35f 0.45f) Assets.Default.Ball world
                if state.ControlMode = PullSling then
                    let impulse =
                        (active.PressPosition - sample.Position) * configuration.PullImpulsePerPixel
                        |> M1Control.clampMagnitude configuration.MaximumImpulse
                    let launchVelocity = center.BodyLinearVelocity + impulse
                    let gravity = (Constants.Physics.GravityDefault * Constants.Engine.Meter2d).V2
                    for index in 1 .. 12 do
                        let time = single index * 0.09f
                        let point = center.BodyCenter + launchVelocity * time + gravity * (0.5f * time * time)
                        M1SceneVisual.sprite
                            (sprintf "Trajectory %02d" index)
                            point
                            (v2Dup (7.0f - single index * 0.25f))
                            3.8f
                            (color 1.0f 0.42f 0.65f (0.9f - single index * 0.05f))
                            Assets.Default.Ball
                            world
            | ControlInactive -> ()

            World.endGroup world
            World.beginGroup "M1 Interface" [] world

            M1SceneVisual.sprite "Control Panel" (v2 0.0f 137.0f) (v2 640.0f 86.0f) 29.0f (color 0.025f 0.04f 0.1f 0.94f) Assets.Default.White world
            M1SceneVisual.sprite "Telemetry Panel" (v2 0.0f -137.0f) (v2 640.0f 86.0f) 29.0f (color 0.025f 0.04f 0.1f 0.94f) Assets.Default.White world
            M1SceneVisual.segment "Playfield Top" (v2 -310.0f playBounds.Max.Y) (v2 310.0f playBounds.Max.Y) 2.0f 28.0f (color 0.25f 0.82f 1.0f 0.45f) world
            M1SceneVisual.segment "Playfield Bottom" (v2 -310.0f playBounds.Min.Y) (v2 310.0f playBounds.Min.Y) 2.0f 28.0f (color 0.25f 0.82f 1.0f 0.45f) world

            let selected selectedValue value label = if selectedValue = value then "[" + label + "]" else label
            if M1SceneVisual.button "Body Legacy" (selected state.Candidate LegacyGraph "Legacy") (v2 -272.0f 134.0f) 82.0f world then
                state <- M1SceneState.rebuild LegacyGraph state.ControlMode state.Room state
            if M1SceneVisual.button "Body Ring" (selected state.Candidate SimplifiedRing "Ring") (v2 -184.0f 134.0f) 82.0f world then
                state <- M1SceneState.rebuild SimplifiedRing state.ControlMode state.Room state
            if M1SceneVisual.button "Body Hull" (selected state.Candidate StableHull "Hull") (v2 -96.0f 134.0f) 82.0f world then
                state <- M1SceneState.rebuild StableHull state.ControlMode state.Room state
            if M1SceneVisual.button "Control Grab" (selected state.ControlMode GrabThrow "Grab") (v2 8.0f 134.0f) 82.0f world then
                state <- M1SceneState.rebuild state.Candidate GrabThrow state.Room state
            if M1SceneVisual.button "Control Pull" (selected state.ControlMode PullSling "Pull") (v2 96.0f 134.0f) 82.0f world then
                state <- M1SceneState.rebuild state.Candidate PullSling state.Room state
            if M1SceneVisual.button "Control Swipe" (selected state.ControlMode SwipeSmack "Swipe") (v2 184.0f 134.0f) 82.0f world then
                state <- M1SceneState.rebuild state.Candidate SwipeSmack state.Room state

            if M1SceneVisual.button "Room Empty" (selected state.Room EmptyToyRoom "Toy") (v2 -270.0f 103.0f) 92.0f world then
                state <- M1SceneState.rebuild state.Candidate state.ControlMode EmptyToyRoom state
            if M1SceneVisual.button "Room Target" (selected state.Room GenerousTargetRoom "Target") (v2 -170.0f 103.0f) 100.0f world then
                state <- M1SceneState.rebuild state.Candidate state.ControlMode GenerousTargetRoom state
            if M1SceneVisual.button "Replay" (if state.LastTrace.Length = 0 then "Replay --" else "Replay trace") (v2 -48.0f 103.0f) 112.0f world && state.LastTrace.Length > 0 then
                let reset = M1SceneState.reset state
                state <-
                    { reset with
                        ReplayIndexOpt = Some 0
                        ReplayCount = state.ReplayCount + 1 }
            if M1SceneVisual.button "Reset" "Reset (R)" (v2 72.0f 103.0f) 100.0f world || World.isKeyboardKeyPressed KeyboardKey.R world then
                state <- M1SceneState.reset state
            if M1SceneVisual.button "Quit" "Quit" (v2 180.0f 103.0f) 74.0f world then
                screen.SetGameplayState Quit world

            World.doText "M1 Title"
                [Entity.Position .= v3 0.0f 165.0f 0.0f
                 Entity.Size .= v3 620.0f 28.0f 0.0f
                 Entity.Elevation .= 31.0f
                 Entity.FontSizing .= Some 13.0f
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.Text .= "M1 • BODY × CONTROL LAB"]
                world
            World.doText "Prompt"
                [Entity.Position .= v3 -150.0f 48.0f 0.0f
                 Entity.Size .= v3 250.0f 24.0f 0.0f
                 Entity.Elevation .= 20.0f
                 Entity.FontSizing .= Some 9.0f
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.Text @= Scene06_M1ControlStudyDispatcher.ControlText state.ControlMode]
                world
            World.doText "Outcome"
                [Entity.Position .= v3 190.0f 52.0f 0.0f
                 Entity.Size .= v3 210.0f 26.0f 0.0f
                 Entity.Elevation .= 20.0f
                 Entity.FontSizing .= Some 11.0f
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.Text @= Scene06_M1ControlStudyDispatcher.OutcomeText state.Outcome]
                world

            let bodyCount = M1Topology.bodyCount state.Candidate
            let jointCount = M1Topology.jointCount state.Candidate
            let reduction = M1Topology.constraintReduction state.Candidate * 100.0f
            let traceText = if state.LastTrace.Length = 0 then "none" else sprintf "%d • %s" state.LastTrace.Length (M1Trace.checksum state.LastTrace)
            World.doText "Telemetry Topology"
                [Entity.Position .= v3 0.0f -111.0f 0.0f
                 Entity.Size .= v3 610.0f 18.0f 0.0f
                 Entity.Elevation .= 30.0f
                 Entity.FontSizing .= Some 7.0f
                 Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                 Entity.Text @= sprintf "%s | bodies %d joints %d | constraint reduction %.1f%% | max speed %.0f" (Scene06_M1ControlStudyDispatcher.CandidateText state.Candidate) bodyCount jointCount reduction configuration.MaximumSpeed]
                world
            World.doText "Telemetry Control"
                [Entity.Position .= v3 0.0f -132.0f 0.0f
                 Entity.Size .= v3 610.0f 18.0f 0.0f
                 Entity.Elevation .= 30.0f
                 Entity.FontSizing .= Some 7.0f
                 Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                 Entity.Text @= sprintf "force %.1f / %.0f | impulse %.1f / %.0f | velocity %.1f | attempts %d hit %d miss %d" (state.LastForce.Length ()) configuration.MaximumForce (state.LastImpulse.Length ()) configuration.MaximumImpulse (center.BodyLinearVelocity.Length ()) state.Attempts state.Hits state.Misses]
                world
            World.doText "Telemetry Trace"
                [Entity.Position .= v3 0.0f -153.0f 0.0f
                 Entity.Size .= v3 610.0f 18.0f 0.0f
                 Entity.Elevation .= 30.0f
                 Entity.FontSizing .= Some 7.0f
                 Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                 Entity.Text @= sprintf "trace %s | replay %A | resets %d replays %d | frame %.2fms physics %.2fms" traceText state.ReplayIndexOpt state.ResetCount state.ReplayCount world.Timers.FrameTimer.Elapsed.TotalMilliseconds world.Timers.PhysicsTimer.Elapsed.TotalMilliseconds]
                world

            screen.SetM1SceneState state world
            World.endGroup world