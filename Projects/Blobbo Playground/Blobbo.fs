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
        member this.GetSpawnPosition world : Vector2 = this.Get (nameof this.SpawnPosition) world
        member this.SetSpawnPosition (value : Vector2) world = this.Set (nameof this.SpawnPosition) value world
        member this.SpawnPosition = lens (nameof this.SpawnPosition) this this.GetSpawnPosition this.SetSpawnPosition

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
         define Entity.SpawnPosition v2Zero
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
                    let posA = B2Bodies.b2Body_GetPosition a
                    let posB = B2Bodies.b2Body_GetPosition b
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
                registerBodyJoint
                    { BodySource = blobbo; BodyIndex = i }
                    { BodySource = blobbo; BodyIndex = j }
                    interContourDistance
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
            let spawnPos = blobbo.GetSpawnPosition world
            let center = blobbo.GetBlobboCenter world
            let delta = spawnPos - center.BodyCenter
            let center' = { center with BodyCenter = spawnPos }
            blobbo.SetBlobboCenter center' world
            let centerBodyId = { BodySource = blobbo; BodyIndex = centerBodyIndex }
            World.setBodyCenter spawnPos.V3 centerBodyId world
            let contour = blobbo.GetBlobboContour world
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
        let size = (blobbo.GetSize world * blobbo.GetScale world).V2
        if contour.Length >= 3 then
            let position = blobbo.GetPosition world
            let points = contour |> Array.map (fun t -> (t.BodyCenter - position.V2) / size)
            let inline normalizeSafe (v : Vector2) =
                let m = v.Magnitude
                if m > 1e-6f then v / m else v
            
            // normalize points to CCW
            let points =
                let signedAreaTimes2 =
                    let mutable acc = 0f
                    for i in 0 .. points.Length - 1 do
                        let p = points[i]
                        let q = points[(i + 1) % points.Length]
                        acc <- acc + (p.X * q.Y - q.X * p.Y)
                    acc
                if signedAreaTimes2 < 0f then Array.rev points else points
            let outwardNormal (edgeDir : Vector2) = v2 edgeDir.Y -edgeDir.X // for CCW polygon, outward normal is right normal

            let lineIntersection (p1 : Vector2) (d1 : Vector2) (p2 : Vector2) (d2 : Vector2) =
                let denom = Vector2.Cross (d1, d2)
                if abs denom < 1e-6f then ValueNone
                else
                    let t = Vector2.Cross (p2 - p1, d2) / denom
                    ValueSome (p1 + d1 * t)

            // expand polygon outward by contourCircleRadius
            let expansion = v2Dup contourCircleRadius / size
            let expanded =
                Array.init points.Length (fun i ->
                    let pPrev = points[(i - 1 + points.Length) % points.Length]
                    let p = points[i]
                    let pNext = points[(i + 1) % points.Length]

                    let dPrev = normalizeSafe (p - pPrev)   // prev edge dir
                    let dNext = normalizeSafe (pNext - p)   // next edge dir

                    let nPrev = outwardNormal dPrev
                    let nNext = outwardNormal dNext

                    // offset lines for adjacent edges, both at distance radius
                    let l1Point = pPrev + nPrev * expansion
                    let l1Dir = dPrev
                    let l2Point = p + nNext * expansion
                    let l2Dir = dNext

                    match lineIntersection l1Point l1Dir l2Point l2Dir with
                    | ValueSome ip -> ip
                    | ValueNone ->
                        // parallel/degenerate fallback: shift current point by averaged outward normal
                        let avg = normalizeSafe (nPrev + nNext)
                        p + avg * expansion
                )

            // round corners on expanded polygon
            let cornerData =
                Array.init expanded.Length (fun i ->
                    let pPrev = expanded[(i - 1 + expanded.Length) % expanded.Length]
                    let p = expanded[i]
                    let pNext = expanded[(i + 1) % expanded.Length]
                    let inVec = p - pPrev
                    let outVec = pNext - p
                    let inLen = inVec.Magnitude
                    let outLen = outVec.Magnitude

                    let maxR = 0.5f * min inLen outLen // clamp radius per-corner by edge lengths so arcs always fit
                    let r = Vector2.Min (expansion, v2Dup maxR)
                    let inDir = inVec.Normalized // points from prev -> p
                    let outDir = outVec.Normalized // points from p -> next
                    let tIn = p - inDir * r // tangent point on incoming edge (approaching p)
                    let tOut = p + outDir * r // tangent point on outgoing edge (leaving p)
                    struct (tIn, p, tOut)
                )

            let commands = Array.zeroCreate (expanded.Length * 2 + 2)
            let struct (_, _, firstOut) = cornerData[0]
            commands[0] <- MoveTo firstOut
            for i in 1 .. expanded.Length do
                let idx = i % expanded.Length
                let struct (tIn, corner, tOut) = cornerData[idx]
                commands[i * 2 - 1] <- LineTo tIn
                commands[i * 2] <- QuadraticCurveTo (corner, tOut)
            commands[expanded.Length * 2 + 1] <- CloseContour

            let tessellation =
                ContourTessellation.make
                    commands
                    (ContourFill.ofColor Color.Aqua)
                    ContourStroke.none
                    v2One
            World.renderContour
                { Transform = blobbo.GetTransform world
                  ClipOpt = ValueNone
                  Tessellation = tessellation } world