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
        member this.GetBlobboCenter world : Vector3 = this.Get (nameof this.BlobboCenter) world
        member this.SetBlobboCenter (value : Vector3) world = this.Set (nameof this.BlobboCenter) value world
        member this.BlobboCenter = lens (nameof this.BlobboCenter) this this.GetBlobboCenter this.SetBlobboCenter
        // Note: we do not store rotation and angular velocity because the contour is made of circles.
        member this.GetBlobboContour world : PhysicsBodyTransform array = this.Get (nameof this.BlobboContour) world
        member this.SetBlobboContour (value : PhysicsBodyTransform array) world = this.Set (nameof this.BlobboContour) value world
        member this.BlobboContour = lens (nameof this.BlobboContour) this this.GetBlobboContour this.SetBlobboContour
        member this.GetBlobboFullness world : single = this.Get (nameof this.BlobboFullness) world
        member this.SetBlobboFullness (value : single) world = this.Set (nameof this.BlobboFullness) value world
        member this.BlobboFullness = lens (nameof this.BlobboFullness) this this.GetBlobboFullness this.SetBlobboFullness
        member this.GetBlobboDistanceJointLength world : single = this.Get (nameof this.BlobboDistanceJointLength) world
        member this.SetBlobboDistanceJointLength (value : single) world = this.Set (nameof this.BlobboDistanceJointLength) value world
        member this.BlobboDistanceJointLength = lens (nameof this.BlobboDistanceJointLength) this this.GetBlobboDistanceJointLength this.SetBlobboDistanceJointLength
        member this.GetLastExplosionTime world : int64 = this.Get (nameof this.LastExplosionTime) world
        member this.SetLastExplosionTime (value : int64) world = this.Set (nameof this.LastExplosionTime) value world
        member this.LastExplosionTime = lens (nameof this.LastExplosionTime) this this.GetLastExplosionTime this.SetLastExplosionTime
        member this.ShootEvent = stoa<Vector3> "Shoot/Event" --> this
        member this.ReviveEvent = stoa<unit> "Revive/Event" --> this

// A blobbo is a soft-body made of a Dynamic center body (this entity, via RigidBodyFacet) surrounded by
// a ring of 32 contour boxes linked with revolute joints (perimeter chain) and distance joints back to
// the center (shape stabilization). The contour bodies and joints are owned directly by this dispatcher
// via RegisterPhysics / UnregisterPhysics rather than being declared as child entities.
type BlobboDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static let internalIndex = -1
    static let contourCount = 32
    static let contourSize = 8f
    static let centerRadius = 8f
    static let spawnScale = contourSize * single contourCount / 8f
    static let contourRadius = MathF.PI * spawnScale / single contourCount // half arc-spacing so bodies just touch
    static let fullnessCapacity = 64.0f // logical water units represented by fullness = 1.0
    static let maxAbsorbPerUpdate = 4
    static let absorbCooldownUpdates = 20L
    static let minRadiusScale = 0.35f
    static let minCenterRadius = 2.0f
    static let baseLinearDamping = 0.2f
    static let baseAngularDamping = Constants.Physics.AngularDampingDefault
    static let resizeLinearDamping = 1.5f
    static let resizeAngularDamping = 1.5f

    // BodyIndex layout:
    //   Constants.Physics.InternalIndex (-1) = center body (owned directly by this dispatcher)
    //   0 .. contourCount-1                  = contour box bodies
    // BodyJointIndex layout:
    //   0 .. contourCount-1                  = revolute (perimeter) joints
    //   contourCount .. 2*contourCount-1     = distance (spring) joints to center

    let computeBoundingBox (blobbo : Entity) world =
        blobbo.GetBlobboContour world
        |> Array.map (fun t -> Box2 (t.BodyCenter.V2 - v2Dup contourSize / 2f, v2Dup contourSize))
        |> Array.reduce _.Combine
        |> fun bounds -> bounds.Box3

    // Even-odd point-in-polygon test on the contour loop in world space.
    let isPointInsideContour (point : Vector2) (contour : PhysicsBodyTransform array) =
        if contour.Length < 3 then false
        else
            let mutable inside = false
            let mutable j = contour.Length - 1
            for i in 0 .. contour.Length - 1 do
                let pi = contour[i].BodyCenter.V2
                let pj = contour[j].BodyCenter.V2
                let intersects =
                    ((pi.Y > point.Y) <> (pj.Y > point.Y)) &&
                    (point.X < (pj.X - pi.X) * (point.Y - pi.Y) / (pj.Y - pi.Y + 0.000001f) + pi.X)
                if intersects then inside <- not inside
                j <- i
            inside

    let contourCentroid (contour : PhysicsBodyTransform array) =
        let mutable sum = v3Zero
        for t in contour do
            sum <- sum + t.BodyCenter
        sum / single contour.Length

    // Fullness maps to area; therefore radius scales by sqrt(fullness).
    // To preserve solver stability, map to a nonzero radius range [minRadiusScale, 1].
    let getRadiusScale fullness =
        let fullness = Math.Clamp (fullness, 0.0f, 1.0f)
        minRadiusScale + (1.0f - minRadiusScale) * MathF.Sqrt fullness

    let getCenterRadius distanceLength =
        max minCenterRadius (centerRadius * distanceLength / spawnScale)

    let getDistanceLength fullness =
        spawnScale * getRadiusScale fullness

    let createCenterBody (center : Vector3) (radius : single) (blobbo : Entity) world =
        let centerBodyId = { BodySource = blobbo; BodyIndex = internalIndex }
        let centerBodyProperties =
            { Enabled = true
              Center = center
              Rotation = Quaternion.Identity
              Scale = v3One
              BodyShape = SphereShape { Radius = radius; TransformOpt = None; PropertiesOpt = None }
              BodyType = Dynamic
              SleepingAllowed = true
              Friction = Constants.Physics.FrictionDefault
              Restitution = 0.333f
              LinearVelocity = v3Zero
              LinearDamping = baseLinearDamping
              AngularVelocity = v3Zero
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
              BodyIndex = internalIndex }
        World.createBody2d centerBodyId centerBodyProperties world

    let setCenterBodyRadius (radius : single) (blobbo : Entity) world =
        let centerBodyId = { BodySource = blobbo; BodyIndex = internalIndex }
        World.setBodyShape (SphereShape { Radius = radius; TransformOpt = None; PropertiesOpt = None }) centerBodyId world

    let resetContourAroundCenter (center : Vector3) (reviveScale : single) (blobbo : Entity) world =
        let boxCount = single contourCount
        let revivedContour =
            Array.init contourCount (fun i ->
                let boxAngle = MathF.Tau * single i / boxCount
                let targetCenter = center + v3 (cos boxAngle * reviveScale) (sin boxAngle * reviveScale) 0f
                let bodyId = { BodySource = blobbo; BodyIndex = i }
                World.setBodyCenter targetCenter bodyId world
                World.setBodyRotation Quaternion.Identity bodyId world
                World.setBodyLinearVelocity v3Zero bodyId world
                World.setBodyAngularVelocity v3Zero bodyId world
                { BodyCenter = targetCenter
                  BodyRotation = Quaternion.Identity
                  BodyLinearVelocity = v3Zero
                  BodyAngularVelocity = v3Zero })
        blobbo.SetBlobboContour revivedContour world

    let setDistanceJointsLength (length : single) (blobbo : Entity) world =
        for i in 0 .. contourCount - 1 do
            let bodyJointId = { BodyJointSource = blobbo; BodyJointIndex = contourCount + i }
            World.setBodyJointDistance length bodyJointId world

    let setBlobboDamping linearDamping angularDamping (blobbo : Entity) world =
        let centerBodyId = { BodySource = blobbo; BodyIndex = internalIndex }
        World.setBodyLinearDamping linearDamping centerBodyId world
        World.setBodyAngularDamping angularDamping centerBodyId world
        for i in 0 .. contourCount - 1 do
            let contourBodyId = { BodySource = blobbo; BodyIndex = i }
            World.setBodyLinearDamping linearDamping contourBodyId world
            World.setBodyAngularDamping angularDamping contourBodyId world

    static member Facets =
        []

    static member Properties =
        [define Entity.Visible true
         define Entity.WorldFluidEmitter Address.empty
         define Entity.BlobboCenter v3Zero
         define Entity.BlobboContour Array.empty
         define Entity.BlobboFullness 1.0f
         nonPersistent Entity.BlobboDistanceJointLength spawnScale
         nonPersistent Entity.LastExplosionTime Int64.MinValue
         nonPersistent Entity.PhysicsMotion ManualMotion // disable automatic Position/Rotation/LinearVelocity/AngularVelocity updates for internalIndex.
         computed Entity.BodyId (fun blobbo _-> { BodySource = blobbo; BodyIndex = internalIndex }) None // points to BlobboCenter
         ]
         
    override _.Register (blobbo, world) =

        // shoot emits logical water back into the world
        World.monitor (fun event world ->
            let blobbo : Entity = event.Subscriber
            let fullness = blobbo.GetBlobboFullness world
            if fullness > 0.0f then
                let center = blobbo.GetBlobboCenter world
                let delta = event.Data - center
                let velocity = if delta.LengthSquared () > 0.000001f then Vector3.Normalize delta * 12.0f else v3Zero
                let p =
                    { FluidParticlePosition = center
                      FluidParticleVelocity = velocity
                      FluidParticleConfig = "Water" }
                match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
                | Some emitter -> World.emitFluidParticles (SArray.singleton p) (emitter.GetFluidEmitterId world) world
                | None -> ()
                blobbo.SetBlobboFullness (max 0.0f (fullness - 1.0f / fullnessCapacity)) world
            Cascade) blobbo.ShootEvent blobbo world

    override _.RegisterPhysics (blobbo, world) =

        // Create the center body directly, then the 32 contour box bodies and their joints.
        let initialDistanceLength = getDistanceLength (blobbo.GetBlobboFullness world)

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
                        let x = cos boxAngle * initialDistanceLength
                        let y = sin boxAngle * initialDistanceLength
                        { BodyCenter = center + v3 x y 0f
                          BodyRotation = Quaternion.Identity
                          BodyLinearVelocity = v3Zero
                          BodyAngularVelocity = v3Zero })
                blobbo.SetBlobboCenter center world
                blobbo.SetBlobboContour contour world

                (center, contour)

        // create the actual physics body for the blobbo center
        let initialCenterRadius = getCenterRadius initialDistanceLength
        createCenterBody center initialCenterRadius blobbo world
        blobbo.SetBlobboDistanceJointLength initialDistanceLength world
        blobbo.SetPerimeter (computeBoundingBox blobbo world) world
        
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
                  Restitution = 0f
                  LinearVelocity = t.BodyLinearVelocity
                  LinearDamping = baseLinearDamping
                  AngularVelocity = t.BodyAngularVelocity
                  AngularDamping = baseAngularDamping
                  AngularFactor = v3One
                  KinematicPushLimitOpt = None
                  Substance = Mass (1f / boxCount)
                  Gravity = GravityWorld
                  CharacterProperties = (PogoSpringCharacterProperties PogoSpringCharacterProperties.defaultProperties)
                  VehicleProperties = VehiclePropertiesAbsent
                  CollisionDetection = Continuous
                  CollisionGroup = -B2Constants.B2_SECRET_COOKIE // fluid particles share this group, so water flows through physically
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
                    jointDef.length <- toPhysics initialDistanceLength
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

        let contourBounds = computeBoundingBox blobbo world
        blobbo.SetPerimeter contourBounds world

        // Keep physical contour size in sync with fullness by adjusting center-distance spring rest length.
        let centerBodyId = { BodySource = blobbo; BodyIndex = internalIndex }
        let fullness = blobbo.GetBlobboFullness world
        let centerEnabled = fullness > 0.0f
        let targetDistanceLength = getDistanceLength fullness
        let currentDistanceLength = blobbo.GetBlobboDistanceJointLength world
        let resizing = abs (targetDistanceLength - currentDistanceLength) > 0.125f
        if resizing then
            let targetCenterRadius = getCenterRadius targetDistanceLength
            setCenterBodyRadius targetCenterRadius blobbo world
            setDistanceJointsLength targetDistanceLength blobbo world
            blobbo.SetBlobboDistanceJointLength targetDistanceLength world

        if resizing then
            setBlobboDamping resizeLinearDamping resizeAngularDamping blobbo world
        elif centerEnabled then
            setBlobboDamping baseLinearDamping baseAngularDamping blobbo world

        World.setBodyEnabled centerEnabled centerBodyId world
        let contourAfter = blobbo.GetBlobboContour world
        if contourAfter.Length = contourCount then
            let centerPos = blobbo.GetBlobboCenter world
            let centerInside = isPointInsideContour centerPos.V2 contourAfter

            if not centerInside && centerEnabled then
                // Collapse: center escapes contour. Disable center collider and spill all logical water.
                blobbo.SetLastExplosionTime world.UpdateTime world
                World.setBodyEnabled false centerBodyId world
                let fullness = blobbo.GetBlobboFullness world
                let spillCount = int (ceil (fullness * fullnessCapacity))
                if spillCount > 0 then
                    let origin = contourCentroid contourAfter
                    match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
                    | Some emitter ->
                        World.emitFluidParticles
                            (SArray.init spillCount (fun _ ->
                                let jitter = v2 (Gen.randomf * 2.0f - 1.0f) (Gen.randomf * 2.0f - 1.0f) * 16.0f
                                let velocity = v3 jitter.X jitter.Y 0.0f
                                { FluidParticlePosition = origin + jitter.V3
                                  FluidParticleVelocity = velocity
                                  FluidParticleConfig = "Water" }))
                            (emitter.GetFluidEmitterId world) world
                    | None -> ()
                blobbo.SetBlobboFullness 0.0f world

        if World.doSubscriptionAny "BlobboRevive" blobbo.ReviveEvent world then
            // Explicit revive action: if there is water, re-wrap contour around center at fullness-scaled radius.
            let contourAfter = blobbo.GetBlobboContour world
            if contourAfter.Length = contourCount && fullness > 0.0f then
                let centerPos = contourCentroid contourAfter
                let reviveScale = getDistanceLength fullness
                let reviveCenterRadius = getCenterRadius reviveScale
                blobbo.SetBlobboCenter centerPos world
                setCenterBodyRadius reviveCenterRadius blobbo world
                resetContourAroundCenter centerPos reviveScale blobbo world
                setDistanceJointsLength reviveScale blobbo world
                blobbo.SetBlobboDistanceJointLength reviveScale world

        // Absorb world water particles inside the contour polygon into logical fullness.
        // Contour bodies use the fluid group so water flows through physically; absorption is purely logical.
        let fullness = blobbo.GetBlobboFullness world
        let cooledDown = world.UpdateTime - blobbo.GetLastExplosionTime world >= absorbCooldownUpdates
        if fullness < 1.0f && cooledDown then
            let contourForAbsorb = blobbo.GetBlobboContour world
            let capacityLeft = int (floor ((1.0f - fullness) * fullnessCapacity))
            let absorbLimit = min maxAbsorbPerUpdate capacityLeft
            if absorbLimit > 0 && contourForAbsorb.Length = contourCount then
                match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
                | Some (emitter : Entity) ->
                    let mutable absorbedCount = 0
                    World.chooseFluidParticles (fun p ->
                        if  absorbedCount < absorbLimit &&
                            isPointInsideContour p.FluidParticlePosition.V2 contourForAbsorb then
                            absorbedCount <- inc absorbedCount
                            ValueNone
                        else ValueSome p) (emitter.GetFluidEmitterId world) world
                    if absorbedCount > 0 then
                        let fullness' = min 1.0f (fullness + single absorbedCount / fullnessCapacity)
                        blobbo.SetBlobboFullness fullness' world
                        // Auto-revive when water is first absorbed while center is disabled.
                        if fullness <= 0.0f && fullness' > 0.0f then
                            let centerPos = contourCentroid (blobbo.GetBlobboContour world)
                            let reviveScale = getDistanceLength fullness'
                            let reviveCenterRadius = getCenterRadius reviveScale
                            blobbo.SetBlobboCenter centerPos world
                            setCenterBodyRadius reviveCenterRadius blobbo world
                            resetContourAroundCenter centerPos reviveScale blobbo world
                            setDistanceJointsLength reviveScale blobbo world
                            blobbo.SetBlobboDistanceJointLength reviveScale world
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