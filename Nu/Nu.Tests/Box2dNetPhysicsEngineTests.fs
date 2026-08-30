// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace Nu.Tests
open System
open System.Numerics
open Box2D.NET
open NUnit.Framework
open Nu
module Box2dNetPhysicsEngineTests =

    let private makeSource name =
        { GsgAddress = atoa (stoa ("Box2dNetPhysicsEngineTests/" + name)) } :> Simulant

    let private makeBodyProperties bodyIndex center radius linearVelocity angularVelocity =
        { Enabled = true
          Center = center
          Rotation = Quaternion.Identity
          Scale = Vector3.One
          BodyType = Dynamic
          BodyShape = SphereShape { Radius = radius; TransformOpt = None; PropertiesOpt = None }
          SleepingAllowed = false
          Friction = 0.0f
          Restitution = 0.0f
          LinearVelocity = linearVelocity
          LinearDamping = 0.0f
          AngularVelocity = angularVelocity
          AngularDamping = 0.0f
          AngularFactor = Vector3.One
          KinematicPushLimitOpt = None
          Substance = Density 1.0f
          Gravity = GravityIgnore
          CharacterProperties = PogoSpringCharacterProperties PogoSpringCharacterProperties.defaultProperties
          VehicleProperties = VehiclePropertiesAbsent
          CollisionDetection = Discrete
          CollisionGroup = 0
          CollisionCategories = 1UL
          CollisionMask = UInt64.MaxValue
          Sensor = false
          BodyIndex = bodyIndex }

    let private createBody source bodyProperties (physicsEngine : PhysicsEngine) =
        let bodyId = { BodySource = source; BodyIndex = bodyProperties.BodyIndex }
        physicsEngine.HandleMessage
            (CreateBodyMessage
                { BodyId = bodyId
                  BodyProperties = bodyProperties })
        bodyId

    let [<Test>] ``Runtime damping messages update Box2D body motion.`` () =
        Nu.init ()
        let physicsEngine = Box2dNetPhysicsEngine.make Vector3.Zero
        try
            let source = makeSource "Damping"
            let bodyId =
                createBody source
                    (makeBodyProperties 0 Vector3.Zero 16.0f (v3 120.0f 0.0f 0.0f) (v3 0.0f 0.0f 6.0f))
                    physicsEngine
            physicsEngine.HandleMessage
                (SetBodyLinearDampingMessage { BodyId = bodyId; LinearDamping = 12.0f })
            physicsEngine.HandleMessage
                (SetBodyAngularDampingMessage { BodyId = bodyId; AngularDamping = 12.0f })
            physicsEngine.TryIntegrate (GameTime.ofUpdates 1L) |> ignore
            let linearVelocity = physicsEngine.GetBodyLinearVelocity bodyId
            let angularVelocity = physicsEngine.GetBodyAngularVelocity bodyId
            Assert.That (linearVelocity.X, Is.GreaterThan(0.0f).And.LessThan(120.0f))
            Assert.That (angularVelocity.Z, Is.GreaterThan(0.0f).And.LessThan(6.0f))
        finally
            physicsEngine.CleanUp ()

    let [<Test>] ``Runtime sphere shape message updates Box2D collision radius.`` () =
        Nu.init ()
        let physicsEngine = Box2dNetPhysicsEngine.make Vector3.Zero
        try
            let source = makeSource "Shape"
            let bodyId =
                createBody source
                    (makeBodyProperties 0 Vector3.Zero 32.0f Vector3.Zero Vector3.Zero)
                    physicsEngine
            let ray = Ray3 (v3 -100.0f 20.0f 0.0f, v3 200.0f 0.0f 0.0f)
            let cast () = physicsEngine.RayCast (ray, UInt64.MaxValue, UInt64.MaxValue, false)
            Assert.That (cast (), Has.Length.EqualTo 1)
            physicsEngine.HandleMessage
                (SetBodyShapeMessage
                    { BodyId = bodyId
                      BodyShape = SphereShape { Radius = 8.0f; TransformOpt = None; PropertiesOpt = None } })
            Assert.That (cast (), Is.Empty)
        finally
            physicsEngine.CleanUp ()

    let [<Test>] ``Runtime distance message updates Box2D distance joint length.`` () =
        Nu.init ()
        let physicsEngine = Box2dNetPhysicsEngine.make Vector3.Zero
        try
            let source = makeSource "Distance"
            let bodyId =
                createBody source
                    (makeBodyProperties 0 (v3 -100.0f 0.0f 0.0f) 4.0f Vector3.Zero Vector3.Zero)
                    physicsEngine
            let bodyId2 =
                createBody source
                    (makeBodyProperties 1 (v3 100.0f 0.0f 0.0f) 4.0f Vector3.Zero Vector3.Zero)
                    physicsEngine
            let bodyJointId = { BodyJointSource = source; BodyJointIndex = 0 }
            let bodyJoint =
                Box2dNetBodyJoint
                    { CreateBodyJoint = fun toPhysics _ body body2 world ->
                        let mutable definition = B2Joints.b2DefaultDistanceJointDef ()
                        definition.``base``.bodyIdA <- body
                        definition.``base``.bodyIdB <- body2
                        definition.length <- toPhysics 200.0f
                        B2Joints.b2CreateDistanceJoint (world, &definition) }
            physicsEngine.HandleMessage
                (CreateBodyJointMessage
                    { BodyJointSource = source
                      BodyJointProperties =
                        { BodyJoint = bodyJoint
                          BodyJointTarget = bodyId
                          BodyJointTarget2 = bodyId2
                          BodyJointEnabled = true
                          BreakingPointOpt = None
                          Broken = false
                          CollideConnected = false
                          BodyJointIndex = 0 } })
            physicsEngine.HandleMessage
                (SetBodyJointDistanceMessage { BodyJointId = bodyJointId; Distance = 50.0f })
            let mutable center = v3 -100.0f 0.0f 0.0f
            let mutable center2 = v3 100.0f 0.0f 0.0f
            for _ in 1 .. 60 do
                match physicsEngine.TryIntegrate (GameTime.ofUpdates 1L) with
                | Some messages ->
                    for message in messages do
                        match message with
                        | BodyTransformMessage transform when transform.BodyId = bodyId -> center <- transform.Center
                        | BodyTransformMessage transform when transform.BodyId = bodyId2 -> center2 <- transform.Center
                        | _ -> ()
                | None -> ()
            Assert.That ((center2 - center).Length (), Is.EqualTo(50.0f).Within(1.0f))
        finally
            physicsEngine.CleanUp ()