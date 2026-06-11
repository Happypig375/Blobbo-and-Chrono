namespace BlobboPlayground
open System
open System.Numerics
open Box2D.NET
open Prime
open Nu


// A water balloon is a soft-body made of a Dynamic center body surrounded by
// a ring of 32 contour bodies linked with revolute joints (perimeter chain) and
// distance joints back to the center (shape stabilization). When the center
// body escapes the contour ring, the balloon pops and emits water particles.
type WaterBalloonDispatcher () =
    inherit Entity2dDispatcherImSim (true, false, false)

    static let internalIndex = -1
    static let contourCount = 32
    static let centerRadius = 8.0f
    static let centerToContourDistance = 32.0f
    static let contourRadius = MathF.PI * centerToContourDistance / single contourCount // half arc-spacing so bodies just touch
    static let baseLinearDamping = 0.2f
    static let baseAngularDamping = Constants.Physics.AngularDampingDefault

    // BodyIndex layout:
    //   Constants.Physics.InternalIndex (-1) = center body (owned directly by this dispatcher)
    //   0 .. contourCount-1                  = contour bodies
    // BodyJointIndex layout:
    //   0 .. contourCount-1                  = revolute (perimeter) joints
    //   contourCount .. 2*contourCount-1     = distance (spring) joints to center

    let computeBoundingBox (blobbo : Entity) world =
        blobbo.GetBlobboContour world
        |> Array.map (fun t -> Box2 (t.BodyCenter - v2Dup contourRadius, v2Dup (contourRadius * 2.0f)))
        |> Array.reduce _.Combine
        |> fun bounds -> bounds.Box3

    // Even-odd point-in-polygon test on the contour loop in world space.
    let isPointInsideContour (point : Vector2) (contour : PhysicsBodyTransform array) =
        if contour.Length < 3 then false
        else
            let mutable inside = false
            let mutable j = contour.Length - 1
            for i in 0 .. contour.Length - 1 do
                let pi = contour[i].BodyCenter
                let pj = contour[j].BodyCenter
                if ((pi.Y > point.Y) <> (pj.Y > point.Y)) &&
                   (point.X < (pj.X - pi.X) * (point.Y - pi.Y) / (pj.Y - pi.Y + 0.000001f) + pi.X) then
                    inside <- not inside
                j <- i
            inside

    let contourCentroid (contour : PhysicsBodyTransform array) =
        let mutable sum = v2Zero
        for t in contour do
            sum <- sum + t.BodyCenter
        sum / single contour.Length


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
    static member Facets = []
    static member Properties =
        [define Entity.Visible true
         define Entity.WorldFluidEmitter Address.empty
         define Entity.BlobboCenter initialBlobboCenter
         define Entity.BlobboContour initialBlobboContour
         nonPersistent Entity.PhysicsMotion ManualMotion // disable automatic Position/Rotation/LinearVelocity/AngularVelocity updates for internalIndex.
         computed Entity.BodyId (fun blobbo _ -> { BodySource = blobbo; BodyIndex = internalIndex }) None // points to BlobboCenter
         ]

    override _.RegisterPhysics (blobbo, world) =

        // Create the center body.
        let centerBodyId = { BodySource = blobbo; BodyIndex = internalIndex }
        let center = blobbo.GetBlobboCenter world
        World.createBody2d centerBodyId
            { Enabled = true
              Center = center.BodyCenter.V3
              Rotation = center.BodyRotation
              Scale = v3One
              BodyShape = SphereShape { Radius = centerRadius; TransformOpt = None; PropertiesOpt = None }
              BodyType = Dynamic
              SleepingAllowed = true
              Friction = Constants.Physics.FrictionDefault
              Restitution = 0.333f
              LinearVelocity = center.BodyLinearVelocity.V3
              LinearDamping = baseLinearDamping
              AngularVelocity = center.BodyAngularVelocity.V3
              AngularDamping = baseAngularDamping
              AngularFactor = v3One
              KinematicPushLimitOpt = None
              Substance = Mass 1f
              Gravity = GravityWorld
              CharacterProperties = (PogoSpringCharacterProperties PogoSpringCharacterProperties.defaultProperties)
              VehicleProperties = VehiclePropertiesAbsent
              CollisionDetection = Continuous
              CollisionGroup = -B2Constants.B2_SECRET_COOKIE
              CollisionCategories = Physics.categorizeCollisionMask "1"
              CollisionMask = Physics.categorizeCollisionMask Constants.Physics.CollisionWildcard
              Sensor = false
              BodyIndex = internalIndex } world

        // Create contour bodies.
        for i in 0 .. contourCount - 1 do
            let t = (blobbo.GetBlobboContour world)[i]
            let bodyId = { BodySource = blobbo; BodyIndex = i }
            World.createBody2d bodyId
                { Enabled = true
                  Center = t.BodyCenter.V3
                  Rotation = t.BodyRotation
                  Scale = v3One
                  BodyShape = SphereShape { Radius = contourRadius; TransformOpt = None; PropertiesOpt = None }
                  BodyType = Dynamic
                  SleepingAllowed = true
                  Friction = Constants.Physics.FrictionDefault
                  Restitution = 0f
                  LinearVelocity = t.BodyLinearVelocity.V3
                  LinearDamping = baseLinearDamping
                  AngularVelocity = t.BodyAngularVelocity.V3
                  AngularDamping = baseAngularDamping
                  AngularFactor = v3One
                  KinematicPushLimitOpt = None
                  Substance = Mass (1f / single contourCount)
                  Gravity = GravityWorld
                  CharacterProperties = (PogoSpringCharacterProperties PogoSpringCharacterProperties.defaultProperties)
                  VehicleProperties = VehiclePropertiesAbsent
                  CollisionDetection = Continuous
                  CollisionGroup = -B2Constants.B2_SECRET_COOKIE // fluid particles share this group, so water flows through physically
                  CollisionCategories = Physics.categorizeCollisionMask "1"
                  CollisionMask = Physics.categorizeCollisionMask Constants.Physics.CollisionWildcard
                  Sensor = false
                  BodyIndex = i } world

        // Create revolute joints linking contour bodies in a closed ring.
        for i in 0 .. contourCount - 1 do
            let next = (i + 1) % contourCount
            let bodyIdA = { BodySource = blobbo; BodyIndex = i }
            let bodyIdB = { BodySource = blobbo; BodyIndex = next }
            let bodyJointId = { BodyJointSource = blobbo; BodyJointIndex = i }
            World.createBodyJoint2d blobbo
                { BodyJoint = Box2dNetBodyJoint { CreateBodyJoint = fun _ _ a b world ->
                    let posA = B2Bodies.b2Body_GetPosition a
                    let posB = B2Bodies.b2Body_GetPosition b
                    let midX = (posA.X + posB.X) * 0.5f
                    let midY = (posA.Y + posB.Y) * 0.5f
                    let mutable jointDef = B2Joints.b2DefaultRevoluteJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.``base``.localFrameA.p <- new _ (midX - posA.X, midY - posA.Y)
                    jointDef.``base``.localFrameB.p <- new _ (midX - posB.X, midY - posB.Y)
                    B2Joints.b2CreateRevoluteJoint (world, &jointDef) }
                  BodyJointTarget = bodyIdA
                  BodyJointTarget2 = bodyIdB
                  BodyJointEnabled = true
                  BreakingPointOpt = None
                  Broken = false
                  CollideConnected = true
                  BodyJointIndex = bodyJointId.BodyJointIndex } world

        // Create distance (spring) joints from each contour body back to the center.
        for i in 0 .. contourCount - 1 do
            let contourBodyId = { BodySource = blobbo; BodyIndex = i }
            let bodyJointId = { BodyJointSource = blobbo; BodyJointIndex = contourCount + i }
            World.createBodyJoint2d blobbo
                { BodyJoint = Box2dNetBodyJoint { CreateBodyJoint = fun toPhysics _ a b world ->
                    let mutable jointDef = B2Joints.b2DefaultDistanceJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.length <- toPhysics centerToContourDistance
                    jointDef.enableSpring <- true
                    jointDef.hertz <- 3f
                    jointDef.dampingRatio <- 1f
                    B2Joints.b2CreateDistanceJoint (world, &jointDef) }
                  BodyJointTarget = centerBodyId
                  BodyJointTarget2 = contourBodyId
                  BodyJointEnabled = true
                  BreakingPointOpt = None
                  Broken = false
                  CollideConnected = false
                  BodyJointIndex = bodyJointId.BodyJointIndex } world

    override _.UnregisterPhysics (blobbo, world) =

        // destroy distance joints (center-to-contour)
        let centerBodyId = { BodySource = blobbo; BodyIndex = internalIndex }
        for i in 0 .. contourCount - 1 do
            let contourBodyId = { BodySource = blobbo; BodyIndex = i }
            let bodyJointId = { BodyJointSource = blobbo; BodyJointIndex = contourCount + i }
            World.destroyBodyJoint2d centerBodyId contourBodyId bodyJointId world

        // destroy revolute joints (perimeter)
        for i in 0 .. contourCount - 1 do
            let next = (i + 1) % contourCount
            let bodyIdA = { BodySource = blobbo; BodyIndex = i }
            let bodyIdB = { BodySource = blobbo; BodyIndex = next }
            let bodyJointId = { BodyJointSource = blobbo; BodyJointIndex = i }
            World.destroyBodyJoint2d bodyIdA bodyIdB bodyJointId world

        // destroy center body
        World.destroyBody2d centerBodyId world

        // destroy contour bodies
        for i in 0 .. contourCount - 1 do
            World.destroyBody2d { BodySource = blobbo; BodyIndex = i } world

    override _.Process (blobbo, world) =

        if world.ContextInitializing then
            let spawnPos = blobbo.GetPosition world
            let center = blobbo.GetBlobboCenter world
            let delta = spawnPos.V2 - center.BodyCenter
            let center' = { center with BodyCenter = spawnPos.V2 }
            blobbo.SetBlobboCenter center' world
            let centerBodyId = { BodySource = blobbo; BodyIndex = internalIndex }
            World.setBodyCenter spawnPos centerBodyId world
            let contour = Array.copy (blobbo.GetBlobboContour world)
            for i in 0 .. contour.Length - 1 do
                let t = contour[i]
                let t' = { t with BodyCenter = t.BodyCenter + delta }
                contour[i] <- t'
                World.setBodyCenter t'.BodyCenter.V3 { BodySource = blobbo; BodyIndex = i } world
            blobbo.SetBlobboContour contour world

        // Track body transforms from physics events.
        let contour =
            let existing = blobbo.GetBlobboContour world
            if existing.Length = contourCount then Array.copy existing
            else Array.zeroCreate contourCount

        for event in World.doSubscriptionToBodyEvents "WaterBalloonBodyEvents" blobbo world do
            match event with
            | BodyTransformData transform when transform.BodyId.BodyIndex = internalIndex ->
                blobbo.SetBlobboCenter
                    { BodyCenter = transform.BodyCenter.V2
                      BodyRotation = transform.BodyRotation
                      BodyLinearVelocity = transform.BodyLinearVelocity.V2
                      BodyAngularVelocity = transform.BodyAngularVelocity.V2 } world
            | BodyTransformData transform when transform.BodyId.BodyIndex >= 0 && transform.BodyId.BodyIndex < contourCount ->
                contour[transform.BodyId.BodyIndex] <-
                    { BodyCenter = transform.BodyCenter.V2
                      BodyRotation = transform.BodyRotation
                      BodyLinearVelocity = transform.BodyLinearVelocity.V2
                      BodyAngularVelocity = transform.BodyAngularVelocity.V2 }
            | _ -> ()

        blobbo.SetBlobboContour contour world

        // Update perimeter for broad-phase queries.
        let contourBounds = computeBoundingBox blobbo world
        blobbo.SetPerimeter contourBounds world

        // Pop check: if the center body escapes the contour ring, emit water particles and disable the center.
        if contour.Length = contourCount then
            let center = blobbo.GetBlobboCenter world
            if not (isPointInsideContour center.BodyCenter contour) then
                // Pop! Emit water particles.
                let centroid = contourCentroid contour
                match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
                | Some emitter ->
                    World.emitFluidParticles
                        (SArray.init 32 (fun _ ->
                            let jitter = v2 (Gen.randomf * 2.0f - 1.0f) (Gen.randomf * 2.0f - 1.0f) * 16.0f
                            { FluidParticlePosition = (centroid + jitter).V3
                              FluidParticleVelocity = v3 jitter.X jitter.Y 0.0f
                              FluidParticleConfig = "Water" }))
                        (emitter.GetFluidEmitterId world) world
                | None -> ()
                // Disable the center body so the balloon goes limp.
                let centerBodyId = { BodySource = blobbo; BodyIndex = internalIndex }
                World.setBodyEnabled false centerBodyId world

    override _.Render (_, blobbo, world) =
        let contour = blobbo.GetBlobboContour world
        if contour.Length >= 3 then
            let position = blobbo.GetPosition world
            let size =
                let s = (blobbo.GetSize world).V2
                v2 (max 0.0001f s.X) (max 0.0001f s.Y)
            let points = contour |> Array.map (fun t -> (t.BodyCenter - position.V2) / size)
            let commands = Array.zeroCreate<ContourCommand> (points.Length + 1)
            commands[0] <- MoveTo points[0]
            for i in 1 .. points.Length - 1 do
                commands[i] <- LineTo points[i]
            commands[points.Length] <- CloseContour
            let tessellation =
                ContourTessellation.make
                    commands
                    (ContourFill.ofColor Color.Red)
                    ContourStroke.none
                    size
            let mutable transform = blobbo.GetTransform world
            transform.Rotation <- Quaternion.Identity
            transform.Scale <- v3One
            World.renderContour
                { Transform = transform
                  ClipOpt = ValueNone
                  Tessellation = tessellation } world