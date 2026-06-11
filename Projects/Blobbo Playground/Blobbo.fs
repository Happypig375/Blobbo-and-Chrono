namespace BlobboPlayground
open System
open System.Numerics
open Box2D.NET
open Prime
open Nu

type PhysicsBodyTransform =
    { BodyCenter : Vector2
      BodyRotation : Quaternion
      BodyLinearVelocity : Vector2
      BodyAngularVelocity : Vector2 }

module [<AutoOpen>] BlobboExtensions =
    type Entity with
        member this.GetWorldFluidEmitter world : Entity Address = this.Get (nameof this.WorldFluidEmitter) world
        member this.SetWorldFluidEmitter (value : Entity Address) world = this.Set (nameof this.WorldFluidEmitter) value world
        member this.WorldFluidEmitter = lens (nameof this.WorldFluidEmitter) this this.GetWorldFluidEmitter this.SetWorldFluidEmitter
        member this.GetBlobboCenter world : PhysicsBodyTransform = this.Get (nameof this.BlobboCenter) world
        member this.SetBlobboCenter (value : PhysicsBodyTransform) world = this.Set (nameof this.BlobboCenter) value world
        member this.BlobboCenter = lens (nameof this.BlobboCenter) this this.GetBlobboCenter this.SetBlobboCenter
        member this.GetBlobboContour world : PhysicsBodyTransform array = this.Get (nameof this.BlobboContour) world
        member this.SetBlobboContour (value : PhysicsBodyTransform array) world = this.Set (nameof this.BlobboContour) value world
        member this.BlobboContour = lens (nameof this.BlobboContour) this this.GetBlobboContour this.SetBlobboContour
        member this.ReviveEvent = stoa<unit> "Revive/Event" --> this

type BlobboDispatcher () =
    inherit Entity2dDispatcherImSim (true, false, false)

    static let centerBodyIndex = -1 // internal index for body joints
    static let centerToContourDistance = 32f
    static let initialBlobboCenter =
        { BodyCenter = v2Zero
          BodyRotation = Quaternion.Identity
          BodyLinearVelocity = v2Zero
          BodyAngularVelocity = v2Zero }
    static let initialBlobboContour =
        let count = 32
        [|for i in 0 .. count - 1 ->
            let angle = single i * MathF.TWO_PI / single count
            { BodyCenter = v2 (cos angle * centerToContourDistance) (sin angle * centerToContourDistance)
              BodyRotation = Quaternion.Identity
              BodyLinearVelocity = v2Zero
              BodyAngularVelocity = v2Zero }
        |]
    static let interContourDistance = Vector2.Distance (initialBlobboContour[0].BodyCenter, initialBlobboContour[1].BodyCenter)
    static let contourCircleRadius = interContourDistance / 2f
    static let centerCircleRadius = contourCircleRadius * 1.5f

    static member Facets = []
    static member Properties =
        [define Entity.WorldFluidEmitter Address.empty
         define Entity.BlobboCenter initialBlobboCenter
         define Entity.BlobboContour initialBlobboContour
         define Entity.AwakeTimeStamp 0
         nonPersistent Entity.PhysicsMotion ManualMotion]

    override _.RegisterPhysics (blobbo, world) =
        let registerPhysicsTransform radius i physicsTransform =
            let bodyId = { BodySource = blobbo; BodyIndex = i }
            let bodyProperties =
                { Enabled = true
                  Center = physicsTransform.BodyCenter.V3
                  Rotation = physicsTransform.BodyRotation
                  Scale = v3One
                  BodyShape = SphereShape { Radius = radius; TransformOpt = None; PropertiesOpt = None }
                  BodyType = Dynamic
                  SleepingAllowed = true
                  Friction = Constants.Physics.FrictionDefault
                  Restitution = 0f
                  LinearVelocity = physicsTransform.BodyLinearVelocity.V3
                  LinearDamping = 0f
                  AngularVelocity = physicsTransform.BodyAngularVelocity.V3
                  AngularDamping = 0f
                  AngularFactor = v3One
                  KinematicPushLimitOpt = None
                  Substance = Density 1f
                  Gravity = GravityWorld
                  CharacterProperties = PogoSpringCharacterProperties PogoSpringCharacterProperties.defaultProperties
                  VehicleProperties = VehiclePropertiesAbsent
                  CollisionDetection = Continuous
                  CollisionGroup = -B2Constants.B2_SECRET_COOKIE // fluid particles share this group, so water flows through physically
                  CollisionCategories = Physics.categorizeCollisionMask "1"
                  CollisionMask = Physics.categorizeCollisionMask Constants.Physics.CollisionWildcard
                  Sensor = false
                  BodyIndex = i }
            World.createBody2d bodyId bodyProperties world
        let existingContour = blobbo.GetBlobboContour world
        Array.iteri (registerPhysicsTransform contourCircleRadius) existingContour
        registerPhysicsTransform centerCircleRadius centerBodyIndex (blobbo.GetBlobboCenter world)

        let registerBodyJoint bodyIdA bodyIdB distance i =
            let bodyJointProperties =
                { BodyJoint = Box2dNetBodyJoint { CreateBodyJoint = fun toPhysics _ a b world ->
                    let mutable jointDef = B2Joints.b2DefaultDistanceJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.length <- toPhysics distance
                    jointDef.enableSpring <- true
                    jointDef.hertz <- 4f
                    jointDef.dampingRatio <- 0.5f
                    B2Joints.b2CreateDistanceJoint (world, &jointDef) }
                  BodyJointTarget = bodyIdA
                  BodyJointTarget2 = bodyIdB
                  BodyJointEnabled = true
                  BreakingPointOpt = None
                  Broken = false
                  CollideConnected = true
                  BodyJointIndex = i }
            World.createBodyJoint2d blobbo bodyJointProperties world
        for i in 0 .. existingContour.Length - 1 do
            for j in i + 1 .. existingContour.Length - 1 do
                let distance = Vector2.Distance (initialBlobboContour[i].BodyCenter, initialBlobboContour[j].BodyCenter)
                registerBodyJoint
                    { BodySource = blobbo; BodyIndex = i }
                    { BodySource = blobbo; BodyIndex = j }
                    distance
                    (existingContour.Length + i * existingContour.Length + j)
            registerBodyJoint
                { BodySource = blobbo; BodyIndex = i }
                { BodySource = blobbo; BodyIndex = centerBodyIndex }
                centerToContourDistance
                i
    override _.UnregisterPhysics (blobbo, world) =
        let existingContour = blobbo.GetBlobboContour world
        for i in 0 .. existingContour.Length - 1 do
            for j in i + 1 .. existingContour.Length - 1 do
                World.destroyBodyJoint2d
                    { BodySource = blobbo; BodyIndex = i }
                    { BodySource = blobbo; BodyIndex = j }
                    { BodyJointSource = blobbo; BodyJointIndex = existingContour.Length + i * existingContour.Length + j }
                    world
            World.destroyBodyJoint2d
                { BodySource = blobbo; BodyIndex = i }
                { BodySource = blobbo; BodyIndex = centerBodyIndex }
                { BodyJointSource = blobbo; BodyJointIndex = i }
                world
        World.destroyBody2d { BodySource = blobbo; BodyIndex = centerBodyIndex } world
        World.destroyBodies2d [for i in 0 .. existingContour.Length - 1 -> { BodySource = blobbo; BodyIndex = i }] world
    override _.Process (blobbo, world) =

        if world.ContextInitializing then
            let spawnPos = blobbo.GetPosition world
            let center = blobbo.GetBlobboCenter world
            let delta = spawnPos.V2 - center.BodyCenter
            let center' = { center with BodyCenter = spawnPos.V2 }
            blobbo.SetBlobboCenter center' world
            let centerBodyId = { BodySource = blobbo; BodyIndex = centerBodyIndex }
            World.setBodyCenter spawnPos centerBodyId world
            let contour = Array.copy (blobbo.GetBlobboContour world)
            for i in 0 .. contour.Length - 1 do
                let t = contour[i]
                let t' = { t with BodyCenter = t.BodyCenter + delta }
                contour[i] <- t'
                World.setBodyCenter t'.BodyCenter.V3 { BodySource = blobbo; BodyIndex = i } world
            blobbo.SetBlobboContour contour world
        let contour = Array.copy (blobbo.GetBlobboContour world)
        for event in World.doSubscriptionToBodyEvents "BlobboBodyEvents" blobbo world do
            match event with
            | BodyTransformData transform when transform.BodyId.BodyIndex = centerBodyIndex ->
                blobbo.SetBlobboCenter
                    { BodyCenter = transform.BodyCenter.V2
                      BodyRotation = transform.BodyRotation
                      BodyLinearVelocity = transform.BodyLinearVelocity.V2
                      BodyAngularVelocity = transform.BodyAngularVelocity.V2 } world
            | BodyTransformData transform when transform.BodyId.BodyIndex >= 0 && transform.BodyId.BodyIndex < contour.Length ->
                contour[transform.BodyId.BodyIndex] <-
                    { BodyCenter = transform.BodyCenter.V2
                      BodyRotation = transform.BodyRotation
                      BodyLinearVelocity = transform.BodyLinearVelocity.V2
                      BodyAngularVelocity = transform.BodyAngularVelocity.V2 }
            | _ -> ()
        blobbo.SetBlobboContour contour world
        let center = blobbo.GetBlobboCenter world
        let perimeter =
            (box2 (center.BodyCenter - v2Dup centerCircleRadius) (v2Dup (centerCircleRadius * 2f)), contour)
            ||> Array.fold (fun perimeter t -> box2 (t.BodyCenter - v2Dup contourCircleRadius) (v2Dup (contourCircleRadius * 2f)) |> perimeter.Combine)
        blobbo.SetPerimeter perimeter.Box3 world

    override _.Render (_, blobbo, world) =
        let contour = blobbo.GetBlobboContour world
        if contour.Length >= 3 then
            let position = blobbo.GetPosition world
            let size = (blobbo.GetSize world).V2
            let points = contour |> Array.map (fun t -> (t.BodyCenter - position.V2) / size)
            let commands = Array.zeroCreate<ContourCommand> (points.Length + 1)
            commands[0] <- MoveTo points[0]
            for i in 1 .. points.Length - 1 do
                commands[i] <- LineTo points[i]
            commands[points.Length] <- CloseContour
            let tessellation =
                ContourTessellation.make
                    commands
                    (ContourFill.ofColor Color.Aqua)
                    ContourStroke.none
                    size
            World.renderContour
                { Transform = blobbo.GetTransform world
                  ClipOpt = ValueNone
                  Tessellation = tessellation } world