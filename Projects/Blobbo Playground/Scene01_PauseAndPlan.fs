namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu
open BlobboPlayground

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState

[<AutoOpen>]
module InstructedPathExtensions =
    type Entity with
        member this.GetBlobbo world : Entity Address = this.Get (nameof Entity.Blobbo) world
        member this.SetBlobbo (value : Entity Address) world = this.Set (nameof Entity.Blobbo) value world
        member this.Blobbo = lens (nameof Entity.Blobbo) this this.GetBlobbo this.SetBlobbo
        member this.GetInstructionPoints world : Vector2 array = this.Get (nameof Entity.InstructionPoints) world
        member this.SetInstructionPoints (value : Vector2 array) world = this.Set (nameof Entity.InstructionPoints) value world
        member this.InstructionPoints = lens (nameof Entity.InstructionPoints) this this.GetInstructionPoints this.SetInstructionPoints
        member this.GetInstructionIndex world : int = this.Get (nameof Entity.InstructionIndex) world
        member this.SetInstructionIndex (value : int) world = this.Set (nameof Entity.InstructionIndex) value world
        member this.InstructionIndex = lens (nameof Entity.InstructionIndex) this this.GetInstructionIndex this.SetInstructionIndex
        member this.GetInstructionDrawing world : bool = this.Get (nameof Entity.InstructionDrawing) world
        member this.SetInstructionDrawing (value : bool) world = this.Set (nameof Entity.InstructionDrawing) value world
        member this.InstructionDrawing = lens (nameof Entity.InstructionDrawing) this this.GetInstructionDrawing this.SetInstructionDrawing
        member this.GetInstructionLeapCooldown world : int = this.Get (nameof Entity.InstructionLeapCooldown) world
        member this.SetInstructionLeapCooldown (value : int) world = this.Set (nameof Entity.InstructionLeapCooldown) value world
        member this.InstructionLeapCooldown = lens (nameof Entity.InstructionLeapCooldown) this this.GetInstructionLeapCooldown this.SetInstructionLeapCooldown
        member this.GetInstructionWasAdvancing world : bool = this.Get (nameof Entity.InstructionWasAdvancing) world
        member this.SetInstructionWasAdvancing (value : bool) world = this.Set (nameof Entity.InstructionWasAdvancing) value world
        member this.InstructionWasAdvancing = lens (nameof Entity.InstructionWasAdvancing) this this.GetInstructionWasAdvancing this.SetInstructionWasAdvancing

type InstructedPathDispatcher () =
    inherit Contour2dDispatcher (false, false, false)

    static let pointDistanceMin = 12.0f
    static let pointDistanceMinSquared = pointDistanceMin * pointDistanceMin
    static let targetDistance = 18.0f
    static let targetDistanceSquared = targetDistance * targetDistance
    static let horizontalDeadzone = 8.0f
    static let leapHeightThreshold = 12.0f
    static let leapHorizontalThreshold = 96.0f
    static let leapCooldownMax = 12

    static let updateTessellation (path : Entity) world =
        let points = path.GetInstructionPoints world
        if Array.length points > 1 then
            let mutable minX = points[0].X
            let mutable minY = points[0].Y
            let mutable maxX = points[0].X
            let mutable maxY = points[0].Y
            for point in points do
                minX <- min minX point.X
                minY <- min minY point.Y
                maxX <- max maxX point.X
                maxY <- max maxY point.Y
            let center = v2 ((minX + maxX) * 0.5f) ((minY + maxY) * 0.5f)
            let size = v2 (max (maxX - minX) 1.0f) (max (maxY - minY) 1.0f)
            let commands =
                points
                |> Array.mapi (fun i point ->
                    let localPoint = v2 ((point.X - center.X) / size.X) ((point.Y - center.Y) / size.Y)
                    if i = 0 then MoveTo localPoint else LineTo localPoint)
            let tessellation =
                ContourTessellation.make
                    commands
                    ContourFill.none
                    (ContourStroke.antiAliased (path.GetStrokeColor world) (path.GetStrokeThickness world))
                    size
            path.SetPosition (center.V3) world
            path.SetSize (size.V3) world
            path.SetOverflow (path.GetStrokeThickness world) world
            path.SetTessellation tessellation world
        else
            path.SetTessellation ContourTessellation.empty world

    static member Properties =
        [define Entity.OverflowAbsolute true
         define Entity.ClipOpt None
         define Entity.StrokeColor (color 0.35f 0.95f 1.0f 1.0f)
         define Entity.StrokeThickness 3.0f
         nonPersistent Entity.Tessellation ContourTessellation.empty
         nonPersistent Entity.Blobbo Address.empty
         define Entity.InstructionPoints [||]
         define Entity.InstructionIndex 1
         define Entity.InstructionDrawing false
         define Entity.InstructionLeapCooldown 0
         define Entity.InstructionWasAdvancing false]

    override this.Update (path, world) =
        match tryResolve (path.GetBlobbo world) path with
        | Some blobbo ->
            let advancing = world.Advancing
            let wasAdvancing = path.GetInstructionWasAdvancing world
            if advancing && not wasAdvancing then
                path.SetInstructionIndex 1 world
                path.SetInstructionLeapCooldown 0 world
            path.SetInstructionWasAdvancing advancing world

            if not advancing then
                let mousePosition : Vector2 = World.getMousePosition2dWorld false world
                if World.isMouseButtonPressed MouseLeft world &&
                   (blobbo.GetBounds world).Contains mousePosition.V3 <> ContainmentType.Disjoint then
                    path.SetInstructionPoints [| (blobbo.GetPosition world).V2 |] world
                    path.SetInstructionIndex 1 world
                    path.SetInstructionDrawing true world
                    path.SetInstructionLeapCooldown 0 world
                if path.GetInstructionDrawing world then
                    if World.isMouseButtonDown MouseLeft world then
                        let points = path.GetInstructionPoints world
                        let drawPoint = mousePosition
                        if Array.length points = 0 then
                            path.SetInstructionPoints [| (blobbo.GetPosition world).V2; drawPoint |] world
                        elif Vector2.DistanceSquared (Array.last points, drawPoint) >= pointDistanceMinSquared then
                            path.SetInstructionPoints (Array.add drawPoint points) world
                    else path.SetInstructionDrawing false world
            else
                path.SetInstructionDrawing false world
                let points = path.GetInstructionPoints world
                if Array.length points > 1 then
                    let blobboPosition = (blobbo.GetPosition world).V2
                    let previousIndex = max 1 (path.GetInstructionIndex world)
                    let rec advanceIndex index =
                        if index < Array.length points && Vector2.DistanceSquared (blobboPosition, points[index]) <= targetDistanceSquared
                        then advanceIndex (inc index)
                        else index
                    let instructionIndex = advanceIndex previousIndex
                    if instructionIndex <> previousIndex then
                        path.SetInstructionLeapCooldown 0 world
                    path.SetInstructionIndex instructionIndex world
                    if instructionIndex < Array.length points then
                        let target = points[instructionIndex]
                        let delta = target - blobboPosition
                        blobbo.SetMovement
                            (if delta.X <= -horizontalDeadzone then Left
                             elif delta.X >= horizontalDeadzone then Right
                             else Still) world
                        let leapCooldown = path.GetInstructionLeapCooldown world
                        let grounded = World.getFluidEmitterFluidGrounded (blobbo.GetFluidEmitterId world) world
                        if grounded &&
                           leapCooldown = 0 &&
                           delta.Y > leapHeightThreshold &&
                           abs delta.X <= leapHorizontalThreshold then
                            World.publish target.V3 blobbo.LeapEvent path world
                            path.SetInstructionLeapCooldown leapCooldownMax world
                        elif leapCooldown > 0 then
                            path.SetInstructionLeapCooldown (dec leapCooldown) world
                    else blobbo.SetMovement Still world
                else blobbo.SetMovement Still world
        | None -> ()
        updateTessellation path world

    override this.Render (_, path, world) =
        if not world.Advancing then
            World.renderContour
                { Transform = path.GetTransform world
                  ClipOpt = path.GetClipOpt world |> Option.toValueOption
                  Tessellation = path.GetTessellation world } world

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type Scene01_PauseAndPlanDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit]

    // here we define the behavior of our gameplay
    override this.Process (_, screen, world) =

        World.beginGroup "Group" [] world
        World.doTileMap "Background"
            [Entity.TileMap .= Assets.Gameplay.Playground] world |> ignore
            
        World.doEntity<FluidEmitter2dDispatcher> "World fluid"
            [Entity.Position |= v3 -60f 0f 0f
             Entity.Size .= v3 640f 400f 0f
             Entity.Elevation .= -2f] world
        if screen.GetSelected world then
            if World.isKeyboardKeyDown KeyboardKey.Grave world then
                let spawn = v2 0f 0f
                World.emitFluidParticles (SArray.init 32 (fun _ ->
                    let jitter = v2 (Gen.randomf * 2f - 1f) (Gen.randomf - 0.5f) * 32.0f
                    { FluidParticlePosition = (spawn + jitter).V3; FluidParticleVelocity = v3Zero; FluidParticleConfig = "Water" }))
                    (world.DeclaredEntity.GetFluidEmitterId world)
                    world
        World.doEntity<BlobboDispatcher> "Blobbo"
            [Entity.Position |= v3 0f 0f 0f
             Entity.WorldFluidEmitter .= world.DeclaredEntity.EntityAddress
             Entity.Elevation .= -1f] world
        let blobbo = world.DeclaredEntity
        World.doEntity<InstructedPathDispatcher> "Instructed Path"
            [Entity.Blobbo .= blobbo.EntityAddress
             Entity.Elevation .= 0.2f] world
        let instructedPath = world.DeclaredEntity
        if screen.GetSelected world then
            let hasInstructedPath = Array.length (instructedPath.GetInstructionPoints world) > 1
            let followingInstructedPath = world.Advancing && hasInstructedPath

            if not followingInstructedPath then
                blobbo.SetMovement
                    (if World.isKeyboardKeyDown KeyboardKey.Left world then Left
                     elif World.isKeyboardKeyDown KeyboardKey.Right world then Right
                     else Still) world

            blobbo.SetAbsorption
                (if World.isKeyboardKeyDown KeyboardKey.Up world then Absorbing
                 elif World.isKeyboardKeyDown KeyboardKey.Down world then Emitting
                 else Equilibrium) world
            
            if World.isKeyboardKeyPressed KeyboardKey.Space world then
                World.setAdvancing (not world.Advancing) world
            if world.Advancing then
                let mousePosition = (World.getMousePosition2dWorld false world).V3
                if not followingInstructedPath then
                    if World.isMouseButtonPressed MouseLeft world then
                        if (blobbo.GetBounds world).Contains mousePosition <> ContainmentType.Disjoint then
                            blobbo.SetChargeTarget (Some mousePosition) world
                        else World.publish mousePosition blobbo.ShootEvent screen world

                    match blobbo.GetChargeTarget world with
                    | Some _ ->
                        if World.isMouseButtonReleased MouseLeft world then
                            blobbo.SetChargeTarget None world
                            World.publish mousePosition blobbo.LeapEvent screen world
                        else blobbo.SetChargeTarget (Some mousePosition) world
                    | None -> ()
                else blobbo.SetChargeTarget None world
            else
                World.doStaticSprite "Overlay" 
                    [Entity.Position .= v3 0f 0f 0.1f
                     Entity.Size .= Constants.Render.DisplayVirtualResolution.V3
                     Entity.Absolute .= true
                     Entity.StaticImage .= Assets.Default.White
                     Entity.Color .= color 0.5f 0.5f 0.5f 0.5f] world |> ignore
            
        // declare quit button
        if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world then
            screen.SetGameplayState Quit world

        World.endGroup world