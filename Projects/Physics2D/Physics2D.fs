﻿namespace Physics2D
open System
open System.Numerics
open Prime
open Nu
open nkast.Aether.Physics2D.Dynamics.Joints

// this extends the Game API to expose GameState as a property.
[<AutoOpen>]
module Physics2DExtensions =
    type Game with
        member this.GetCarAcceleration world : float32 = this.Get (nameof Game.CarAcceleration) world
        member this.SetCarAcceleration (value : float32) world = this.Set (nameof Game.CarAcceleration) value world
        member this.CarAcceleration = lens (nameof Game.CarAcceleration) this this.GetCarAcceleration this.SetCarAcceleration
        member this.GetCarWheelJoint world : WheelJoint option = this.Get (nameof Game.CarWheelJoint) world
        member this.SetCarBackWheelJoint (value : WheelJoint option) world = this.Set (nameof Game.CarWheelJoint) value world
        member this.CarWheelJoint = lens (nameof Game.CarWheelJoint) this this.GetCarWheelJoint this.SetCarBackWheelJoint
        
// this is the dispatcher that customizes the top-level behavior of our game.
type Physics2DDispatcher () =
    inherit GameDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Game.CarAcceleration 0f
         define Game.CarWheelJoint None]

    // here we define the game's top-level behavior
    override this.Process (game, world) =

        // declare Enclosure screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let _ =
            World.beginScreen<DemoScreenDispatcher> Simulants.EnclosureScreen.Name
                (Simulants.EnclosureScreen.GetExists world && Simulants.EnclosureScreen.GetSelected world) behavior
                [Screen.CameraPositionDefault .= CameraAbsolute (v2 60f 0f)
                 Screen.NextScreen .= Desire Simulants.RacecourseScreen] world
        World.beginGroup Simulants.SceneGroup [] world

        // define border
        let _ =
            World.doBlock2d Simulants.BorderEntity // A block uses static physics by default - it does not react to forces or collisions.
                [Entity.Size .= v3 500f 350f 0f
                 Entity.BodyShape .= ContourShape // The body shape handles collisions and is independent of how it's displayed.
                    { Links = // A contour shape, unlike other shapes, is hollow.
                        [|v3 -0.5f 0.5f 0f // Zero is the entity's center, one is the entity's size in positive direction.
                          v3 0.5f 0.5f 0f
                          v3 0.5f -0.5f 0f
                          v3 -0.5f -0.5f 0f|]
                      Closed = true // The last point connects to the first point.
                      // There can be multiple shapes per body, TransformOpt and PropertiesOpt 
                      TransformOpt = None
                      PropertiesOpt = None }
                 // Continuous collision detection adds additional checks between frame positions
                 // against high velocity objects tunneling through thin borders.
                 Entity.CollisionDetection .= Continuous
                 // Collision categories is a binary mask, defaulting to "1" (units place).
                 // The border is set to be in a different category, "10" (twos place)
                 // because we define fans later to not collide with the border.
                 // Meanwhile, unless we change the collision mask (Entity.CollisionMask),
                 // all entites default to collide with "*" (i.e. all collision categories).
                 Entity.CollisionCategories .= "10"
                 Entity.Elevation .= -1f // Draw order of the same elevation prioritizes entities with lower vertical position for 2D games.
                 Entity.StaticImage .= Assets.Gameplay.SkyBoxFront] world

        // define agent
        let (agentBody, _) =
            World.doCharacter2d "Agent"
                [Entity.GravityOverride .= None // characters have 3x gravity by default, get rid of it
                 ] world
        
        // Keyboard controls for agent
        if world.ContextScreen.GetSelected world then
            if World.isKeyboardKeyDown KeyboardKey.A world then
                World.applyBodyForce (
                    if World.getBodyGrounded agentBody world then v3 -500f 0f 0f
                    else v3 -250f 0f 0f) None agentBody world
            if World.isKeyboardKeyDown KeyboardKey.D world then
                World.applyBodyForce (
                    if World.getBodyGrounded agentBody world then v3 500f 0f 0f
                    else v3 250f 0f 0f) None agentBody world
            if World.isKeyboardKeyDown KeyboardKey.W world then
                // Jump or fly
                World.applyBodyForce (
                    if World.getGravity2d world = v3Zero then
                        v3 0f 200f 0f
                    elif World.getBodyGrounded agentBody world then
                        -50f * World.getGravity2d world
                    else v3Zero) None agentBody world
            if World.isKeyboardKeyDown KeyboardKey.S world then
                // Glide down
                World.applyBodyForce (v3 0f -100f 0f) None agentBody world
        
        World.endGroup world
        World.endScreen world
        // set the above screen as the initial screen
        if game.GetSelectedScreenOpt world = None then
            game.SetDesiredScreen (Desire world.DeclaredScreen) world
        
        // declare Racecourse screen
        let _ =
            World.beginScreen<DemoScreenDispatcher> Simulants.RacecourseScreen.Name 
                (Simulants.RacecourseScreen.GetExists world && Simulants.RacecourseScreen.GetSelected world) behavior
                [Screen.CameraPositionDefault .= CameraTracking (Address.makeFromString $"~/{Simulants.SceneGroup}/Car")
                 Screen.NextScreen .= Desire Simulants.EnclosureScreen] world
        World.beginGroup Simulants.SceneGroup [] world

        World.doStaticSprite Simulants.BorderEntity
            [Entity.Size .= v3 500f 350f 0f
             Entity.Position .= v3 -60f 0f 0f
             // Absolute positioning makes this display at the same screen location regardless of the eye position.
             Entity.Absolute .= true
             Entity.Elevation .= -1f
             Entity.StaticImage .= Assets.Gameplay.SkyBoxFront] world

        // define racecourse
        let objectScale = 16f
        let racecourse =
            [|v2 -20f 5f
              v2 -20f 0f
              v2 20f 0f
              v2 25f 0.25f
              v2 30f 1f
              v2 35f 4f
              v2 40f 0f
              v2 45f 0f
              v2 50f -1f
              v2 55f -2f
              v2 60f -2f
              v2 65f -1.25f
              v2 70f 0f
              v2 75f 0.3f
              v2 80f 1.5f
              v2 85f 3.5f
              v2 90f 0f
              v2 95f -0.5f
              v2 100f -1f
              v2 105f -2f
              v2 110f -2.5f
              v2 115f -1.3f
              v2 120f 0f
              v2 160f 0f
              v2 159f -10f
              v2 201f -10f
              v2 200f 0f
              v2 240f 0f
              v2 250f 5f
              v2 250f -10f
              v2 270f -10f
              v2 270f 0f
              v2 310f 0f
              v2 310f 5f|] |> Array.map (fun p -> p.V3 * objectScale)
        let _ =
            World.doBlock2d "Racecourse"
                [Entity.Size .= v3 1f 1f 0f
                 Entity.BodyShape .= ContourShape { Links = racecourse; Closed = false; TransformOpt = None; PropertiesOpt = None }
                 // Don't let the car wheels fall through the ground
                 Entity.CollisionDetection .= Continuous
                 // Don't collide with Fan dragging
                 Entity.CollisionCategories .= "10"]
                world
        for (p1, p2) in Array.pairwise racecourse do
            World.doStaticSprite $"Racecourse {p1} -> {p2}"
                [Entity.Position .= (p1 + p2) / 2f
                 Entity.Size .= v3 (p2 - p1).Magnitude 2f 0f
                 Entity.Rotation .= Quaternion.CreateLookAt2d (p2 - p1).V2
                 Entity.StaticImage .= Assets.Default.Black] world

        // declare car
        let carMaxSpeed = 50f
        let carSpawnPosition = v3 0f 30f 0f
        let carPoints = [|
            v2 -2.5f 0.92f
            v2 -2.375f 1.46f
            v2 -0.58f 1.92f
            v2 0.46f 1.92f
            v2 2.5f 1.17f
            v2 2.5f 0.795f
            v2 2.3f 0.67f
            v2 -2.25f 0.65f|]
        let carPointsBox = Box2.Enclose carPoints
        let carGetRelativePosition p = (p - carPointsBox.Center) / carPointsBox.Size
        let _ =
            World.doBox2d "Car"
                [Entity.BodyShape .= PointsShape {
                    Points = Array.map (carGetRelativePosition >> _.V3) carPoints
                    Profile = Convex
                    TransformOpt = None
                    PropertiesOpt = None }
                 Entity.StaticImage .= Assets.Gameplay.Car
                 Entity.Position .= carSpawnPosition
                 Entity.Size .= carPointsBox.Size.V3 * objectScale
                 Entity.Substance .= Density 4f
                 Entity.Friction .= 0.2f
                 ] world
        for (relation, position, density, frequency, friction, maxTorque) in
            [("Back", v2 -1.709f 0.78f, 0.8f, 5f, 0.9f, 20f)
             ("Front", v2 1.54f 0.8f, 1f, 8.5f, 0.2f, 10f)] do
            let wheelRelativePosition = (carGetRelativePosition position * carPointsBox.Size).V3 * objectScale
            let _ =
                World.doBall2d $"Wheel {relation}"
                    [Entity.StaticImage .= Assets.Gameplay.Wheel
                     Entity.Position .= carSpawnPosition + wheelRelativePosition
                     Entity.Size .= v3Dup 1f * objectScale
                     Entity.Substance .= Density (density * 2f)
                     Entity.Friction .= friction
                     Entity.Elevation .= 0.1f] world
            World.doBodyJoint2d $"Wheel {relation} joint"
                [Entity.BodyJoint .= TwoBodyJoint2d {
                    CreateTwoBodyJoint = fun _ _ car wheel ->
                        // A wheel joint fixes relative position of two bodies, labelled body A and body B,
                        // where body B is positionally anchored relative to body A, can exhibit
                        // spring movement along an axis (i.e. wheel suspension), and can rotate freely.
                        let wheelJoint =
                            WheelJoint (car, wheel, wheel.Position, new _(0f, 1.2f), true,
                                Frequency = frequency, DampingRatio = 0.85f, MaxMotorTorque = maxTorque)
                        if relation = "Back" then game.SetCarBackWheelJoint (Some wheelJoint) world
                        wheelJoint }
                 Entity.BodyJointTarget .= Address.makeFromString "^/Car"
                 Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/Wheel {relation}")
                 Entity.CollideConnected .= false] world |> ignore
            // Mutation is required to modify properties of the body joint.
            if world.ContextScreen.GetSelected world then
                match game.GetCarWheelJoint world with
                | Some wheelJoint ->
                    let acceleration = game.GetCarAcceleration world
                    wheelJoint.MotorSpeed <- 
                        float32 (sign acceleration) * Math.SmoothStep(0f, carMaxSpeed, abs acceleration)
                    wheelJoint.MotorEnabled <- abs wheelJoint.MotorSpeed >= carMaxSpeed * 0.06f
                | None -> ()
            ()
        
        // Keyboard controls for car
        if world.ContextScreen.GetSelected world then
            if World.isKeyboardKeyDown KeyboardKey.A world then
                game.CarAcceleration.Map (fun a -> min (a + 2.0f * world.ClockTime) 1f) world
            elif World.isKeyboardKeyDown KeyboardKey.D world then
                game.CarAcceleration.Map (fun a -> max (a - 2.0f * world.ClockTime) -1f) world
            elif World.isKeyboardKeyPressed KeyboardKey.S world then
                game.SetCarAcceleration 0f world
            else game.CarAcceleration.Map (fun a -> a - float32 (sign a) * 2.0f * world.ClockTime) world
        
        // declare teeter board
        let (teeter, _) =
            World.doBox2d "Teeter"
                [Entity.Position .= v3 140f 1f 0f * objectScale
                 Entity.Size .= v3 20f 0.5f 0f * objectScale
                 Entity.StaticImage .= Assets.Default.Paddle
                 Entity.Substance .= Density 1f
                 Entity.CollisionDetection .= Continuous] world
        let _ =
            World.doBodyJoint2d "Teeter joint"
                [Entity.BodyJoint .= TwoBodyJoint2d { CreateTwoBodyJoint = fun _ _ a b ->
                    World.applyBodyAngularImpulse (v3 0f 0f 100f) teeter world
                    RevoluteJoint (a, b, b.Position, b.Position, true,
                        LimitEnabled = true, LowerLimit = -8.0f * MathF.PI / 180.0f,
                        UpperLimit = 8.0f * MathF.PI / 180.0f) }
                 Entity.BodyJointTarget .= Address.makeFromString "^/Racecourse"
                 Entity.BodyJointTarget2Opt .= Some (Address.makeFromString "^/Teeter")
                 Entity.CollideConnected .= false] world

        // declare bridge
        for i in 0 .. 20 do
            if i < 20 then
                World.doBox2d $"Bridge {i}" [
                    Entity.Size .= v3 2f 0.25f 0f * objectScale
                    Entity.Position .= v3 (161f + 2f * float32 i) -0.125f 0f * objectScale
                    Entity.Friction .= 0.6f
                    Entity.StaticImage .= Assets.Default.Paddle
                    Entity.CollisionDetection .= Continuous
                    Entity.Substance .= Density 1f] world |> ignore
            World.doBodyJoint2d $"Bridge {i} Link" [
                Entity.BodyJoint .= TwoBodyJoint2d {
                    CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                        let p = if i < 20 then b.Position - toPhysicsV2 (v3 objectScale 0f 0f)
                                else a.Position + toPhysicsV2 (v3 objectScale 0f 0f)
                        RevoluteJoint (a, b, p, p, true) }
                Entity.BodyJointTarget .= Address.makeFromString (if i = 0 then "^/Racecourse" else $"^/Bridge {i-1}")
                Entity.BodyJointTarget2Opt .= Some (Address.makeFromString (if i < 20 then $"^/Bridge {i}" else "^/Racecourse"))
                ] world |> ignore

        // declare boxes
        for i in 0 .. 2 do
            World.doBox2d $"Box {i}" [
                Entity.Position .= v3 220f (0.5f + float32 i) 0f * objectScale
                Entity.Size .= v3Dup objectScale
                Entity.Substance .= Density 1f] world |> ignore

        World.endGroup world
        World.endScreen world

        // handle Alt+F4 when not in editor
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world