namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu
open BlobboPlayground

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type Scene05_HeaterCoolerDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit
         define Screen.HeldEntity Address.empty]

    // here we define the behavior of our gameplay
    override this.Process (_, screen, world) =

        if screen.GetSelected world then
            World.beginGroup "Group" [] world
            
            // declare border
            World.doBlockBody2d "Border"
                [Entity.Size .= (World.getDisplayVirtualResolution ()).V3
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
                
            // declare world fluid emitter
            World.doEntity<FluidEmitter2dDispatcher> "World fluid"
                [Entity.Size .= v3 640f 400f 0f] world
            let fluidEmitter = world.DeclaredEntity

            // =================== Smoke Chamber ===================
            // A walled-off area in the upper-center that Blobbo cannot enter.
            // Only smoke particles (converted from water by the heater) can rise through.
            // Layout: x in [-144, 144], y in [-64, 136], open at the bottom

            // left wall
            World.doBoxBody2d "ChamberLeft"
                [Entity.Position .= v3 -148f 36f 0f
                 Entity.Size .= v3 8f 216f 0f
                 Entity.BodyType .= Static
                 Entity.StaticImage .= Assets.Default.Label] world |> ignore

            // right wall
            World.doBoxBody2d "ChamberRight"
                [Entity.Position .= v3 148f 36f 0f
                 Entity.Size .= v3 8f 216f 0f
                 Entity.BodyType .= Static
                 Entity.StaticImage .= Assets.Default.Label] world |> ignore

            // top wall
            World.doBoxBody2d "ChamberTop"
                [Entity.Position .= v3 0f 144f 0f
                 Entity.Size .= v3 304f 8f 0f
                 Entity.BodyType .= Static
                 Entity.StaticImage .= Assets.Default.Label] world |> ignore

            // =================== Heater ===================
            // A sensor body spanning the bottom entrance of the chamber.
            // Converts water particles to smoke and fires Heat events on overlapping water containers.
            World.doBoxBody2d "Heater"
                [Entity.Position .= v3 0f -68f 0f
                 Entity.Size .= v3 280f 12f 0f
                 Entity.BodyType .= Static
                 Entity.Sensor .= true
                 Entity.StaticImage .= Assets.Default.Label] world |> ignore
            let heater = world.DeclaredEntity

            // =================== Rotating Blade ===================
            // A dynamic fan blade pinned to a static anchor at the center of the smoke chamber.
            // Smoke particles push against the blade, causing it to rotate.

            // static anchor at blade center
            World.doOrbBody2d "BladeAnchor"
                [Entity.Position .= v3 0f 36f 0f
                 Entity.Size .= v3 4f 4f 0f
                 Entity.BodyType .= Static
                 Entity.Sensor .= true
                 Entity.Visible .= false] world |> ignore
            let bladeAnchor = world.DeclaredEntity

            // blade body 1 (horizontal orientation)
            World.doBoxBody2d "BladeH"
                [Entity.Position .= v3 0f 36f 0f
                 Entity.Size .= v3 96f 6f 0f
                 Entity.BodyType .= Dynamic
                 Entity.StaticImage .= Assets.Default.Label
                 Entity.MountOpt .= None] world |> ignore
            let bladeH = world.DeclaredEntity

            // blade body 2 (vertical orientation)
            World.doBoxBody2d "BladeV"
                [Entity.Position .= v3 0f 36f 0f
                 Entity.Size .= v3 6f 96f 0f
                 Entity.BodyType .= Dynamic
                 Entity.StaticImage .= Assets.Default.Label
                 Entity.MountOpt .= None] world |> ignore
            let bladeV = world.DeclaredEntity

            // weld joint: lock both blades together in a cross (+) shape
            World.doBodyJoint2d "BladeWeld"
                [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ _ a b world ->
                    let mutable jointDef = Box2D.NET.B2Joints.b2DefaultWeldJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.``base``.localFrameB.q <- Box2D.NET.B2MathFunction.b2MakeRot MathF.PI_OVER_2
                    Box2D.NET.B2Joints.b2CreateWeldJoint (world, &jointDef) }
                 Entity.BodyJointTarget .= bladeH.EntityAddress
                 Entity.BodyJointTarget2 .= bladeV.EntityAddress
                 Entity.CollideConnected .= false] world |> ignore

            // revolute joint: pin the welded blade assembly to the static anchor so it can rotate but stays in place
            World.doBodyJoint2d "BladePin"
                [Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ _ a b world ->
                    let mutable jointDef = Box2D.NET.B2Joints.b2DefaultRevoluteJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    Box2D.NET.B2Joints.b2CreateRevoluteJoint (world, &jointDef) }
                 Entity.BodyJointTarget .= bladeAnchor.EntityAddress
                 Entity.BodyJointTarget2 .= bladeH.EntityAddress
                 Entity.CollideConnected .= false] world |> ignore

            // =================== Cooler ===================
            // A sensor body at the top of the chamber. Converts smoke particles back to water,
            // which then falls outside the chamber, preventing water from entering the smoke region.
            World.doBoxBody2d "Cooler"
                [Entity.Position .= v3 0f 140f 0f
                 Entity.Size .= v3 280f 12f 0f
                 Entity.BodyType .= Static
                 Entity.Sensor .= true
                 Entity.StaticImage .= Assets.Default.Label] world |> ignore
            let cooler = world.DeclaredEntity

            // =================== Game Entities ===================

            // water balloons with WaterContainerFacet
            World.doEntity<WaterBalloonDispatcher> "Balloon1"
                [Entity.Position .= v3 -90f -120f 0f
                 Entity.WorldFluidEmitter .= fluidEmitter.EntityAddress] world
            let balloon1 = world.DeclaredEntity
            World.doEntity<WaterBalloonDispatcher> "Balloon2"
                [Entity.Position .= v3 90f -120f 0f
                 Entity.WorldFluidEmitter .= fluidEmitter.EntityAddress] world
            let balloon2 = world.DeclaredEntity

            // the player-controlled Blobbo
            World.doEntity<BlobboDispatcher> "Blobbo"
                [Entity.Position .= v3 0f -120f 0f
                 Entity.WorldFluidEmitter .= fluidEmitter.EntityAddress
                 Entity.FacetNames .= set [nameof FeelerFacet]] world
            let blobbo = world.DeclaredEntity

            // =================== Mouse Interaction ===================
            let eyeBounds = World.getEye2dBounds world
            let raw = World.getMousePosition2dWorld false world
            let mousePosition = v3 (max eyeBounds.Min.X (min eyeBounds.Max.X raw.X)) (max eyeBounds.Min.Y (min eyeBounds.Max.Y raw.Y)) 0f
            if blobbo.GetTouched world then
                screen.SetHeldEntity blobbo.EntityAddress world
            elif World.isMouseButtonUp MouseLeft world then
                screen.SetHeldEntity Address.empty world
            if screen.GetHeldEntity world <> Address.empty then
                World.doOrbBody2d "Mouse"
                    [Entity.Position @= mousePosition
                     Entity.Visible .= false
                     Entity.Sensor .= true] world |> ignore
                World.doBodyJoint2d "Mouse joint"
                    [Entity.BodyJointTarget .= stoa "^/Mouse"
                     Entity.BodyJointTarget2 .= screen.GetHeldEntity world
                     Entity.BodyJoint |= Box2dNetBodyJoint { CreateBodyJoint = fun _ _ a b world ->
                        let mutable jointDef = Box2D.NET.B2Joints.b2DefaultDistanceJointDef ()
                        jointDef.``base``.bodyIdA <- a
                        jointDef.``base``.bodyIdB <- b
                        jointDef.length <- 0f
                        jointDef.enableLimit <- true
                        jointDef.minLength <- 0f
                        jointDef.maxLength <- 0f
                        Box2D.NET.B2Joints.b2CreateDistanceJoint (world, &jointDef)
                        }] world |> ignore

            // =================== Heater Mechanics ===================
            // Convert water particles that drift into the heater zone to rising smoke.
            let heaterBounds = heater.GetBounds world
            World.chooseFluidParticles (fun particle ->
                if particle.FluidParticleConfig = "Water" &&
                   heaterBounds.Contains particle.FluidParticlePosition <> ContainmentType.Disjoint then
                    ValueSome
                        { particle with
                            FluidParticleConfig = "Smoke"
                            FluidParticleVelocity = v3 (Gen.randomf - 0.5f) (Gen.randomf * 2.0f + 1.5f) 0.0f * 3.0f }
                else ValueSome particle)
                (fluidEmitter.GetFluidEmitterId world) world

            // Fire Heat event on water containers that overlap the heater zone.
            // This causes them to convert stored water content to smoke particles.
            let eventTrace = EventTrace.debug "Heater" "Process" "" EventTrace.empty
            for container in [blobbo; balloon1; balloon2] do
                let containerPos = container.GetPosition world
                let dx = abs (containerPos.X - 0f)
                let dy = abs (containerPos.Y - (-68f))
                if dx < 150f && dy < 40f then
                    World.publishPlus () container.HeatEvent eventTrace container true false world

            // Debug: spawn water with grave key (moved lower so it enters heater zone naturally)
            if World.isKeyboardKeyDown KeyboardKey.Grave world then
               let spawn = v2 0f -80f
               World.emitFluidParticles (SArray.init 32 (fun _ ->
                   let jitter = v2 (Gen.randomf * 2f - 1f) (Gen.randomf - 0.5f) * 32.0f
                   { FluidParticlePosition = (spawn + jitter).V3; FluidParticleVelocity = v3Zero; FluidParticleConfig = "Water" }))
                (fluidEmitter.GetFluidEmitterId world)
                world

            // =================== Cooler Mechanics ===================
            // Convert smoke particles that reach the top of the chamber back to water.
            // The water falls downward outside the chamber, unable to re-enter.
            let coolerBounds = cooler.GetBounds world
            World.chooseFluidParticles (fun particle ->
                if particle.FluidParticleConfig = "Smoke" &&
                   coolerBounds.Contains particle.FluidParticlePosition <> ContainmentType.Disjoint then
                    ValueSome
                        { particle with
                            FluidParticleConfig = "Water"
                            FluidParticleVelocity = v3 (Gen.randomf - 0.5f) (Gen.randomf * -2.0f - 1.5f) 0.0f * 3.0f }
                else ValueSome particle)
                (fluidEmitter.GetFluidEmitterId world) world

            // =================== Pause / Overlay / Revive ===================
            if screen.GetSelected world then
                if World.isKeyboardKeyPressed KeyboardKey.Space world then
                    World.setTimeAdvancing (not world.TimeAdvancing) world
                if world.TimeAdvancing then ()
                else
                    World.doStaticSprite "Overlay" 
                        [Entity.Position .= v3 0f 0f 0.1f
                         Entity.Size .= (World.getDisplayVirtualResolution ()).V3
                         Entity.Absolute .= true
                         Entity.StaticImage .= Assets.Default.White
                         Entity.Color .= color 0.5f 0.5f 0.5f 0.5f] world |> ignore
                if World.isKeyboardKeyPressed KeyboardKey.Enter world then
                    World.publish () blobbo.ReviveEvent blobbo world

            // declare quit button
            if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"; Entity.Elevation .= 10f] world then
                screen.SetGameplayState Quit world

            World.endGroup world
