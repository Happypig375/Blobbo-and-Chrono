namespace BlobboPlayground
open System
open System.Numerics
open Box2D.NET
open Prime
open Nu

type PhysicsBodyTransform =
    { BodyCenter : Vector3
      BodyRotation : Quaternion
      BodyLinearVelocity : Vector3
      BodyAngularVelocity : Vector3 }

module [<AutoOpen>] BlobboExtensions =
    type Entity with
        member this.GetWorldFluidEmitter world : Entity Address = this.Get (nameof this.WorldFluidEmitter) world
        member this.SetWorldFluidEmitter (value : Entity Address) world = this.Set (nameof this.WorldFluidEmitter) value world
        member this.WorldFluidEmitter = lens (nameof this.WorldFluidEmitter) this this.GetWorldFluidEmitter this.SetWorldFluidEmitter
        member this.GetAbsorbingWater world : bool = this.Get (nameof this.AbsorbingWater) world
        member this.SetAbsorbingWater (value : bool) world = this.Set (nameof this.AbsorbingWater) value world
        member this.AbsorbingWater = lens (nameof this.AbsorbingWater) this this.GetAbsorbingWater this.SetAbsorbingWater
        member this.GetBlobboCenter world : Vector3 = this.Get (nameof this.BlobboCenter) world
        member this.SetBlobboCenter (value : Vector3) world = this.Set (nameof this.BlobboCenter) value world
        member this.BlobboCenter = lens (nameof this.BlobboCenter) this this.GetBlobboCenter this.SetBlobboCenter
        // Note: we do not store rotation and angular velocity because the contour is made of circles.
        member this.GetBlobboContour world : PhysicsBodyTransform array = this.Get (nameof this.BlobboContour) world
        member this.SetBlobboContour (value : PhysicsBodyTransform array) world = this.Set (nameof this.BlobboContour) value world
        member this.BlobboContour = lens (nameof this.BlobboContour) this this.GetBlobboContour this.SetBlobboContour
        member this.ShootEvent = stoa<Vector3> "Shoot/Event" --> this

// A blobbo is a soft-body made of a Dynamic center body (this entity, via RigidBodyFacet) surrounded by
// a ring of 32 contour boxes linked with revolute joints (perimeter chain) and distance joints back to
// the center (shape stabilization). The contour bodies and joints are owned directly by this dispatcher
// via RegisterPhysics / UnregisterPhysics rather than being declared as child entities.
type BlobboDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static let internalIndex = -1
    static let contourCount = 32
    static let contourSize = 8f
    static let spawnScale = contourSize * single contourCount / 8f
    static let contourRadius = MathF.PI * spawnScale / single contourCount // half arc-spacing so bodies just touch
    static let minBlobboSize = 3
    static let contourStrokeColor = Color.Cyan
    static let contourStrokeThickness = 2.0f

    // BodyIndex layout:
    //   Constants.Physics.InternalIndex (-1) = center body (owned directly by this dispatcher)
    //   0 .. contourCount-1                  = contour box bodies
    // BodyJointIndex layout:
    //   0 .. contourCount-1                  = revolute (perimeter) joints
    //   contourCount .. 2*contourCount-1     = distance (spring) joints to center

    let computeBoundingBox (blobbo : Entity) world =
        blobbo.GetBlobboContour world
        |> Array.map (fun t -> Box2 (t.BodyCenter.V2 - v2Dup contourSize, v2Dup contourSize * 2f))
        |> Array.reduce _.Combine
        |> fun bounds -> bounds.Box3

    static member Facets =
        []

    static member Properties =
        [define Entity.Visible true
         define Entity.WorldFluidEmitter Address.empty
         define Entity.AbsorbingWater false
         define Entity.BlobboCenter v3Zero
         define Entity.BlobboContour Array.empty]
         
    override _.Register (blobbo, world) =

        // eject out-of-bounds particles back to the world emitter
        World.monitor (fun event world ->
            let blobbo : Entity = event.Subscriber
            match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
            | Some emitter ->
                World.emitFluidParticles
                    (event.Data.OutOfBoundsParticles |> SArray.map (fun p -> { p with FluidParticleConfig = "Water" }))
                    (emitter.GetFluidEmitterId world) world
            | None -> ()
            Cascade) blobbo.FluidEmitterUpdateEvent blobbo world

        // shoot a particle toward the target position when enough are present in the body
        World.monitor (fun event world ->
            let blobbo : Entity = event.Subscriber
            let mutable i = 0
            let mutable shoot = ValueNone
            World.chooseFluidParticles (fun p ->
                i <- inc i
                if i = minBlobboSize then
                    shoot <- ValueSome { p with FluidParticleVelocity = p.FluidParticleVelocity + (event.Data - p.FluidParticlePosition) * 2f; FluidParticleConfig = "Water" }
                    ValueNone
                else ValueSome p) (blobbo.GetFluidEmitterId world) world
            match shoot with
            | ValueSome p ->
                match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
                | Some emitter -> World.emitFluidParticles (SArray.singleton p) (emitter.GetFluidEmitterId world) world
                | None -> ()
            | ValueNone -> ()
            Cascade) blobbo.ShootEvent blobbo world

    override _.RegisterPhysics (blobbo, world) =

        // Create the center body directly, then the 32 contour box bodies and their joints.

        // initialize BlobboCenter / BlobboContour to the default ring layout if not yet set
        let center, contour =
            let existingContour = blobbo.GetBlobboContour world
            if existingContour.Length = contourCount then
                (blobbo.GetBlobboCenter world, existingContour)
            else
                let center = blobbo.GetPerimeterCenter world
                let boxCount = single contourCount
                let contour =
                    Array.init contourCount (fun i ->
                        let boxAngle = MathF.Tau * single i / boxCount
                        let x = cos boxAngle * spawnScale
                        let y = sin boxAngle * spawnScale
                        { BodyCenter = center + v3 x y 0f
                          BodyRotation = Quaternion.Identity
                          BodyLinearVelocity = v3Zero
                          BodyAngularVelocity = v3Zero })
                blobbo.SetBlobboCenter center world
                blobbo.SetBlobboContour contour world

                // create the actual physics body for the blobbo center
                let centerBodyId = { BodySource = blobbo; BodyIndex = internalIndex }
                let centerBodyProperties =
                        { Enabled = true
                          Center = center
                          Rotation = Quaternion.Identity
                          Scale = v3One
                          BodyShape = SphereShape { Radius = 8f; TransformOpt = None; PropertiesOpt = None }
                          BodyType = Dynamic
                          SleepingAllowed = true
                          Friction = Constants.Physics.FrictionDefault
                          Restitution = 0.333f
                          LinearVelocity = v3Zero
                          LinearDamping = 0f
                          AngularVelocity = v3Zero
                          AngularDamping = Constants.Physics.AngularDampingDefault
                          AngularFactor = v3One
                          KinematicPushLimitOpt = None
                          Substance = Mass 1f
                          Gravity = GravityWorld
                          CharacterProperties = (PogoSpringCharacterProperties PogoSpringCharacterProperties.defaultProperties)
                          VehicleProperties = VehiclePropertiesAbsent
                          CollisionDetection = Continuous
                          CollisionGroup = 0
                          CollisionCategories = Physics.categorizeCollisionMask "1"
                          CollisionMask = Physics.categorizeCollisionMask Constants.Physics.CollisionWildcard
                          Sensor = false
                          BodyIndex = internalIndex }
                World.createBody2d centerBodyId centerBodyProperties world
                blobbo.SetPerimeter (computeBoundingBox blobbo world) world
                (center, contour)

        // create contour box bodies using the serialized contour transforms
        let boxCount = single contourCount
        for i in 0 .. contourCount - 1 do
            let t = contour[i]
            let bodyId = { BodySource = blobbo; BodyIndex = i }
            let bodyProperties =
                { Enabled = true
                  Center = t.BodyCenter
                  Rotation = t.BodyRotation
                  Scale = v3One
                  BodyShape = SphereShape { Radius = contourRadius; TransformOpt = None; PropertiesOpt = None }
                  BodyType = Dynamic
                  SleepingAllowed = true
                  Friction = Constants.Physics.FrictionDefault
                  Restitution = 0.333f
                  LinearVelocity = t.BodyLinearVelocity
                  LinearDamping = 0f
                  AngularVelocity = t.BodyAngularVelocity
                  AngularDamping = Constants.Physics.AngularDampingDefault
                  AngularFactor = v3One
                  KinematicPushLimitOpt = None
                  Substance = Mass (1f / boxCount)
                  Gravity = GravityWorld
                  CharacterProperties = (PogoSpringCharacterProperties PogoSpringCharacterProperties.defaultProperties)
                  VehicleProperties = VehiclePropertiesAbsent
                  CollisionDetection = Continuous
                  CollisionGroup = 0
                  CollisionCategories = Physics.categorizeCollisionMask "1"
                  CollisionMask = Physics.categorizeCollisionMask Constants.Physics.CollisionWildcard
                  Sensor = false
                  BodyIndex = i }
            World.createBody2d bodyId bodyProperties world

        // create revolute joints linking contour boxes in a closed ring
        for i in 0 .. contourCount - 1 do
            let next = (i + 1) % contourCount
            let bodyIdA = { BodySource = blobbo; BodyIndex = i }
            let bodyIdB = { BodySource = blobbo; BodyIndex = next }
            let bodyJointId = { BodyJointSource = blobbo; BodyJointIndex = i }
            let bodyJointProperties =
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
                  BodyJointIndex = bodyJointId.BodyJointIndex }
            World.createBodyJoint2d blobbo bodyJointProperties world

        // create distance (spring) joints from each contour box back to the center
        let centerBodyId = { BodySource = blobbo; BodyIndex = internalIndex }
        for i in 0 .. contourCount - 1 do
            let contourBodyId = { BodySource = blobbo; BodyIndex = i }
            let bodyJointId = { BodyJointSource = blobbo; BodyJointIndex = contourCount + i }
            let bodyJointProperties =
                { BodyJoint = Box2dNetBodyJoint { CreateBodyJoint = fun toPhysics _ a b world ->
                    let mutable jointDef = B2Joints.b2DefaultDistanceJointDef ()
                    jointDef.``base``.bodyIdA <- a
                    jointDef.``base``.bodyIdB <- b
                    jointDef.length <- toPhysics spawnScale
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
                  BodyJointIndex = bodyJointId.BodyJointIndex }
            World.createBodyJoint2d blobbo bodyJointProperties world

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

    override _.Update (blobbo, world) =
        let contour =
            let contour = blobbo.GetBlobboContour world
            if contour.Length = contourCount then Array.copy contour
            else Array.zeroCreate contourCount
        for event in World.doSubscriptionToBodyEvents "BlobboBodyEvents" blobbo world do
            match event with
            | BodyTransformData transform when transform.BodyId.BodyIndex = internalIndex ->
                blobbo.SetBlobboCenter transform.BodyCenter world
            | BodyTransformData transform when transform.BodyId.BodyIndex >= 0 && transform.BodyId.BodyIndex < contourCount ->
                contour[transform.BodyId.BodyIndex] <-
                    { BodyCenter = transform.BodyCenter
                      BodyRotation = transform.BodyRotation
                      BodyLinearVelocity = transform.BodyLinearVelocity
                      BodyAngularVelocity = transform.BodyAngularVelocity }
            | _ -> ()
        blobbo.SetBlobboContour contour world

        blobbo.SetPerimeter (computeBoundingBox blobbo world) world

        if false then
            // update center to average of particle positions
            let mutable newPosition = v3Zero
            let mutable particleCount = 0
            World.chooseFluidParticles (fun p ->
                newPosition <- newPosition + p.FluidParticlePosition
                particleCount <- inc particleCount
                ValueSome p) (blobbo.GetFluidEmitterId world) world
            if particleCount > 0 then
                blobbo.SetPosition (newPosition / single particleCount) world

        // absorb nearby world-emitter particles into the blobbo body
        if blobbo.GetAbsorbingWater world then
            let bounds = blobbo.GetBounds world
            let maxParticles = 10
            let absorbed = ResizeArray maxParticles
            match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
            | Some (emitter : Entity) ->
                World.chooseFluidParticles (fun p ->
                    if bounds.Contains p.FluidParticlePosition <> ContainmentType.Disjoint && absorbed.Count < maxParticles then
                        absorbed.Add p; ValueNone
                    else ValueSome p) (emitter.GetFluidEmitterId world) world
                World.emitFluidParticles
                    (SArray.init absorbed.Count (fun i -> { absorbed[i] with FluidParticleConfig = "Oil" }))
                    (blobbo.GetFluidEmitterId world) world
            | None -> ()

        // eject excess particles back to the world emitter when contracting
        let mutable i = 0
        let bounds = blobbo.GetBounds world
        let maxParticles = 10
        let ejected = ResizeArray maxParticles
        match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
        | Some (emitter : Entity) ->
            World.chooseFluidParticles (fun p ->
                i <- inc i
                if i >= minBlobboSize && bounds.Contains p.FluidParticlePosition <> ContainmentType.Disjoint && ejected.Count < maxParticles then
                    ejected.Add p; ValueNone
                else ValueSome p) (blobbo.GetFluidEmitterId world) world
            if ejected.Count > 0 then
                World.emitFluidParticles
                    (SArray.init ejected.Count (fun i -> ejected[i]))
                    (emitter.GetFluidEmitterId world) world
        | None -> ()

    override _.Render (_, blobbo, world) =
        let contour = blobbo.GetBlobboContour world
        if contour.Length >= 3 then
            let position = blobbo.GetPosition world
            let size =
                let size = (blobbo.GetSize world).V2
                v2 (max 0.0001f size.X) (max 0.0001f size.Y)
            let points = contour |> Array.map (fun t -> (t.BodyCenter - position).V2 / size)
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
            let mutable transform = blobbo.GetTransform world
            transform.Rotation <- Quaternion.Identity
            transform.Scale <- v3One
            World.renderContour
                { Transform = transform
                  ClipOpt = ValueNone
                  Tessellation = tessellation } world