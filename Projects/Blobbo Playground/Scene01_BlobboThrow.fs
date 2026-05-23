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
        member this.GetBlobboHeld world : bool = this.Get (nameof Screen.BlobboHeld) world
        member this.SetBlobboHeld (value : bool) world = this.Set (nameof Screen.BlobboHeld) value world
        member this.BlobboHeld = lens (nameof Screen.BlobboHeld) this this.GetBlobboHeld this.SetBlobboHeld

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type Scene01_BlobboThrowDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit
         define Screen.BlobboHeld false]

    // here we define the behavior of our gameplay
    override this.Process (_, screen, world) =

        World.beginGroup "Group" [] world
        // declare border
        World.doBlockBody2d "Border"
            [Entity.Size .= Constants.Render.DisplayVirtualResolution.V3
             Entity.BodyShape .= ContourShape
                 { Links =
                     [|v3 -0.5f 0.5f 0f
                       v3 0.5f 0.5f 0f
                       v3 0.5f -0.5f 0f
                       v3 -0.5f -0.5f 0f|]
                   Closed = true
                   TransformOpt = None
                   PropertiesOpt = None }
             Entity.Elevation .= -1f
             Entity.StaticImage .= Assets.Gameplay.Background] world |> ignore
            
        World.doEntity<FluidEmitter2dDispatcher> "World fluid"
            [Entity.Position |= v3 -60f 0f 0f
             Entity.Size .= v3 640f 400f 0f] world
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
             Entity.WorldFluidEmitter .= world.DeclaredEntity.EntityAddress] world
        let blobbo = world.DeclaredEntity
             
        let mousePosition = (World.getMousePosition2dWorld false world).V3
        let (isDown, justDown) =
            World.doFeeler "Mouse"
                [Entity.Position @= mousePosition
                 Entity.Elevation .= 1f
                 Entity.FacetNames .= set [nameof RigidBodyFacet]
                 Entity.Sensor .= true
                ] world
        let pickupPerimeter =
            let perimeter = blobbo.GetPerimeter world
            if blobbo.GetBlobboForm world = Flattened
            then box3 (perimeter.Min - v3Dup 24f) (perimeter.Size + v3Dup 48f)
            else perimeter
        if justDown && pickupPerimeter.Contains mousePosition = ContainmentType.Contains then
            if blobbo.GetBlobboForm world = Flattened then
                World.publish () blobbo.ReviveEvent blobbo world
            screen.SetBlobboHeld true world
        elif not isDown then
            screen.SetBlobboHeld false world
        if screen.GetBlobboHeld world && blobbo.GetBlobboForm world = Upright then
            World.doBodyJoint2d "Mouse joint"
                [Entity.BodyJointTarget .= stoa "^/Mouse"
                 Entity.BodyJointTarget2 .= stoa "^/Blobbo"
                 Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ toPhysicsV2 a b world ->
                    let mousePosition = toPhysicsV2 mousePosition
                    let mutable jointDef = Box2D.NET.B2Joints.b2DefaultDistanceJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.``base``.localFrameA.p <- Box2D.NET.B2Bodies.b2Body_GetLocalPoint (a, mousePosition)
                    jointDef.``base``.localFrameB.p <- Box2D.NET.B2Bodies.b2Body_GetLocalPoint (b, mousePosition)
                    jointDef.length <- 0f
                    jointDef.enableSpring <- true
                    jointDef.hertz <- 5f
                    jointDef.dampingRatio <- 1f
                    Box2D.NET.B2Joints.b2CreateDistanceJoint (world, &jointDef)
                    }] world |> ignore
                    

        if screen.GetSelected world then
            
            if World.isKeyboardKeyPressed KeyboardKey.Space world then
                World.setAdvancing (not world.Advancing) world
            if world.Advancing then ()
            else
                World.doStaticSprite "Overlay" 
                    [Entity.Position .= v3 0f 0f 0.1f
                     Entity.Size .= Constants.Render.DisplayVirtualResolution.V3
                     Entity.Absolute .= true
                     Entity.StaticImage .= Assets.Default.White
                     Entity.Color .= color 0.5f 0.5f 0.5f 0.5f] world |> ignore

            if World.isKeyboardKeyPressed KeyboardKey.Enter world then
                World.publish () blobbo.ReviveEvent blobbo world
            
        // declare quit button
        if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"; Entity.Elevation .= 10f] world then
            screen.SetGameplayState Quit world

        World.endGroup world