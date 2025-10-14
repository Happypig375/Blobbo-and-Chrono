namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu
open nkast.Aether.Physics2D.Dynamics.Joints
open BlobboPlayground

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit
type CameraPosition = CameraAbsolute of Vector2 | CameraTracking of Entity Address

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetCameraPosition world : CameraPosition option = this.Get (nameof Screen.CameraPosition) world
        member this.SetCameraPosition (value : CameraPosition option) world = this.Set (nameof Screen.CameraPosition) value world
        member this.CameraPosition = lens (nameof Screen.CameraPosition) this this.GetCameraPosition this.SetCameraPosition
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState
        member this.GetDraggedEntity world : (Entity * Vector3 * BodyType) option = this.Get (nameof Screen.DraggedEntity) world
        member this.SetDraggedEntity (value : (Entity * Vector3 * BodyType) option) world = this.Set (nameof Screen.DraggedEntity) value world
        member this.DraggedEntity = lens (nameof Screen.DraggedEntity) this this.GetDraggedEntity this.SetDraggedEntity
        member this.GetMouseDragTarget world : Map<Entity, Entity> = this.Get (nameof Screen.MouseDragTarget) world
        member this.SetMouseDragTarget (value : Map<Entity, Entity>) world = this.Set (nameof Screen.MouseDragTarget) value world
        member this.MouseDragTarget = lens (nameof Screen.MouseDragTarget) this this.GetMouseDragTarget this.SetMouseDragTarget
        member this.GetSoftBodyContour world : Map<BodyId, Entity> = this.Get (nameof Screen.SoftBodyContour) world
        member this.SetSoftBodyContour (value : Map<BodyId, Entity>) world = this.Set (nameof Screen.SoftBodyContour) value world
        member this.SoftBodyContour = lens (nameof Screen.SoftBodyContour) this this.GetSoftBodyContour this.SetSoftBodyContour

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.CameraPosition None
         define Screen.DraggedEntity None
         define Screen.MouseDragTarget Map.empty 
         define Screen.SoftBodyContour Map.empty
         define Screen.GameplayState Quit]

    // here we define the behavior of our gameplay
    override this.Process (_, screen, world) =

        World.beginGroup "Group" [] world
        World.doTileMap "Background"
            [Entity.TileMap .= Assets.Gameplay.Background] world |> ignore

        World.doBox2d "Box"
            [Entity.FacetNames .= Set.ofList [nameof StaticSpriteFacet; nameof RewindableFacet]
             Entity.Position |= v3 -90f 0f 0f
             Entity.Size .= v3Dup 16f
             Entity.BodyType .= Dynamic
             Entity.StaticImage .= Assets.Default.StaticSprite
             Entity.BodyShape .= BoxShape { Size = v3 1f 1f 0f; PropertiesOpt = None; TransformOpt = None }
             Entity.Substance .= Mass 1f
             Entity.CollisionDetection .= Continuous] world |> ignore
            
        // The Process method is run even for unselected screens because the entity hierarchy
        // defined in code still needs to be preserved across screen switching.
        // This allows entities in one screen to modify entities in another screen.
        // We have to check if the current screen is selected,
        // otherwise we would run keyboard and mouse handlers even for unselected screens!
        if screen.GetSelected world then

            if World.isKeyboardKeyPressed KeyboardKey.Space world then
                world.DeclaredEntity.SetRewindPreview (Some world.GameTime) world
            if World.isKeyboardKeyDown KeyboardKey.Space world then
                world.DeclaredEntity.RewindPreview.Map (Option.map (fun r -> r - world.GameDelta - world.GameDelta)) world
            match world.DeclaredEntity.GetRewindPreview world with
            | Some rewindPreview when World.isKeyboardKeyUp KeyboardKey.Space world ->
                World.publish rewindPreview world.DeclaredEntity.RewindEvent world.DeclaredEntity world
                world.DeclaredEntity.SetRewindPreview None world
            | _ -> ()

        World.doEntity<FluidEmitter2dDispatcher> "World fluid"
            [Entity.Position |= v3 -60f 0f 0f
             Entity.Size .= v3 640f 400f 0f
             Entity.FluidParticleRadius .= 5f
             Entity.GravityOverride .= Some (v3 0f -1f 0f)
             Entity.LinearDamping .= 0.9f] world
        if screen.GetSelected world then
            if World.isKeyboardKeyDown KeyboardKey.Grave world then
                let spawn = v2 0f 0f
                World.emitFluidParticles (SArray.init 32 (fun _ ->
                    let jitter = v2 (Gen.randomf * 2f - 1f) (Gen.randomf - 0.5f) * 32.0f
                    { FluidParticlePosition = (spawn + jitter).V3; FluidParticleVelocity = v3Zero; GravityOverride = ValueNone }))
                    (world.DeclaredEntity.GetFluidEmitterId world)
                    world
        World.doEntity<BlobboDispatcher> "Blobbo"
            [Entity.Position |= v3 0f 0f 0f
             Entity.WorldFluidEmitter .= world.DeclaredEntity.EntityAddress] world
        if screen.GetSelected world then
            world.DeclaredEntity.SetMovement
                (if World.isKeyboardKeyDown KeyboardKey.Left world then Left
                 elif World.isKeyboardKeyDown KeyboardKey.Right world then Right
                 else Still) world
            world.DeclaredEntity.SetAbsorbing (World.isKeyboardKeyDown KeyboardKey.Up world) world

        World.endGroup world