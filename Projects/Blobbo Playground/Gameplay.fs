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
            
        // The Process method is run even for unselected screens because the entity hierarchyAdd a comment on line R71Add diff commentMarkdown input:  edit mode selected.WritePreviewAdd a suggestionHeadingBoldItalicQuoteCodeLinkUnordered listNumbered listTask listMentionReferenceSaved repliesAdd FilesPaste, drop, or click to add filesCancelCommentStart a reviewReturn to code
        // defined in code still needs to be preserved across screen switching.
        // This allows entities in one screen to modify entities in another screen.
        // We have to check if the current screen is selected,
        // otherwise we would run keyboard and mouse handlers even for unselected screens!
        if screen.GetSelected world then

            // Camera control
            let resolveCamera () =
                match screen.GetCameraPosition world |> Option.defaultValue (CameraTracking (Address.makeFromString "~/Group/Blobbo Center")) with
                | CameraAbsolute position -> position
                | CameraTracking relation ->
                    match tryResolve relation screen with
                    | Some e when e.GetExists world ->
                        e.GetPosition(world).V2 +
                            v2 100f 60f // Menu offset (X = 60) + Car lookahead (X = 40) + Make objects spawn above ground (Y = 60)
                    | _ -> v2Zero
            if World.isKeyboardKeyDown KeyboardKey.Left world then
                screen.SetCameraPosition (resolveCamera () - v2 1f 0f |> CameraAbsolute |> Some) world
            if World.isKeyboardKeyDown KeyboardKey.Right world then
                screen.SetCameraPosition (resolveCamera () + v2 1f 0f |> CameraAbsolute |> Some) world
            if World.isKeyboardKeyDown KeyboardKey.Up world then
                screen.SetCameraPosition (resolveCamera () + v2 0f 1f |> CameraAbsolute |> Some) world
            if World.isKeyboardKeyDown KeyboardKey.Down world then
                screen.SetCameraPosition (resolveCamera () - v2 0f 1f |> CameraAbsolute |> Some) world
            if World.isKeyboardKeyDown KeyboardKey.Home world then
                screen.SetCameraPosition None world
            // an eye can render even with no screen. the eye is NOT a camera.
            // it's just a field that specifies where rendering happens.
            // it is up to the user to set the eye to follow a camera.
            World.setEye2dCenter (resolveCamera ()) world

            // Mouse dragging - picking up the entity

            // In Nu, while the physics engine subsystems abstract over 2d and 3d to conform
            // to a 3d interface using Vector3, mouse and spatial subsystems have no utility
            // for such abstraction - they need more specificity. As a result, getMousePostion2dWorld
            // return Vector2, getEntities2dAtPoint takes Vector2, while Entity.Position and
            // rayCastBodies2d expect Vector3. .V3 is used to convert Vector2 to Vector3, .V2 for vice versa.
            // That's the art of API design - sometimes the types are more specific and concrete,
            // while sometimes the types are less specific because it keeps things more general.
            let mousePosition = (World.getMousePosition2dWorld false world).V3
            let setDraggedEntity (entity : Entity) =
                let relativePosition =
                    (mousePosition - entity.GetPosition world).Transform (entity.GetRotation world).Inverted
                screen.SetDraggedEntity (Some (entity, relativePosition, entity.GetBodyType world)) world
                entity.SetBodyType Dynamic world // Only dynamic bodies react to forces by the mouse joint below.
            if World.isMouseButtonPressed MouseLeft world then
                let physicsAnchors = screen.GetMouseDragTarget world
                // (new _()) specifies a new set which is just the temporary container to hold the queried entities.
                // Optimizations can reuse the same set for different queries.
                for entity in World.getEntities2dAtPoint mousePosition.V2 (new _()) world do
                    let entity = Map.tryFind entity physicsAnchors |> Option.defaultValue entity
                    // Check rigid body facet existence to confirm the body type property's validity before reading it
                    if entity.Has<RigidBodyFacet> world
                    && entity.GetVisible world 
                    && screen.GetDraggedEntity world = None // Don't change more than one body to dynamic physics
                    then setDraggedEntity entity
                if screen.GetDraggedEntity world = None then // No entity found via direct point test
                    // Raycast entities to see if mouse location is inside a soft body enclosed area, then drag it
                    let rayUp =
                        World.rayCastBodies2d (ray3 mousePosition (v3Up * 100f)) -1 false world
                        |> Seq.map _.BodyShapeIntersected.BodyId
                        |> Seq.choose (screen.GetSoftBodyContour world).TryFind
                        |> Set
                    let rayDown =
                        World.rayCastBodies2d (ray3 mousePosition (v3Down * 100f)) -1 false world
                        |> Seq.map _.BodyShapeIntersected.BodyId
                        |> Seq.choose (screen.GetSoftBodyContour world).TryFind
                        |> Set
                    let intersection = Set.intersect rayUp rayDown
                    if Set.notEmpty intersection then
                        setDraggedEntity (Set.minElement intersection)

            // Mouse dragging - moving the entity
            match screen.GetDraggedEntity world with
            | Some (draggedEntity, _, draggedBodyType) when World.isMouseButtonUp MouseLeft world ->
                screen.SetDraggedEntity None world
                draggedEntity.SetBodyType draggedBodyType world
            | Some (draggedEntity, relativePosition, draggedBodyType) ->
                // declare sensor for mouse body
                World.doSphere2d "MouseSensor" // A sphere uses static physics by default.
                    [Entity.BodyShape .= SphereShape
                        { Radius = 0.1f
                          // A sensor body never collides with another body.
                          PropertiesOpt = Some { BodyShapeProperties.empty with SensorOpt = Some true }
                          TransformOpt = None }
                     Entity.Visible .= false
                     // Re-initialization of the entity position is required every frame, necessitating the dynamic property operator.
                     Entity.Position @= mousePosition] world |> ignore
                let mouseSensor = world.DeclaredEntity

                // declare distance joint for mouse body
                let mouseJoint = world.ContextGroup / "MouseJoint"
                World.doBodyJoint2d mouseJoint.Name
                    // A relative address can be specified by relating two entities directly using their EntityAddresses.
                    // Relative addresses can safeguard against dragging entities across different hierarchies in the Gaia editor.
                    [Entity.BodyJointTarget .= Address.relate mouseJoint.EntityAddress draggedEntity.EntityAddress
                     // Though, in code, we usually can just specify the absolute address directly.
                     Entity.BodyJointTarget2Opt .= Some mouseSensor.EntityAddress
                     Entity.BreakingPoint .= infinityf // never drop the entity while dragging
                     Entity.BodyJoint .= TwoBodyJoint2d
                        { CreateTwoBodyJoint = fun _ toPhysicsV2 a b ->
                            // Convert mouse position (Vector2) to world position (Vector3) to physics engine position (Aether.Physics2D Vector2)
                            let mousePosition = toPhysicsV2 mousePosition
                            // Give dynamic bodies flick behavior, give static or kinematic bodies weld behavior.
                            if draggedBodyType = Dynamic then
                                // Use true to supply physics engine position as world coordinates which are converted to local body positions.
                                DistanceJoint (a, b, mousePosition, mousePosition, true, Frequency = 1.5f, DampingRatio = 0.5f)
                            else WeldJoint (a, b, mousePosition, mousePosition, true) }] world |> ignore

                // for distance joint, apply damping to body in order to stabilize it while dragged
                draggedEntity.LinearVelocity.Map ((*) 0.9f) world
                draggedEntity.AngularVelocity.Map ((*) 0.9f) world

                let draggedPosition = relativePosition.Transform (draggedEntity.GetRotation world) + draggedEntity.GetPosition world
                // visualise the mouse joint
                World.doBlock2d "MouseJointVisual"
                    [// Update position, size and rotation every frame to match the two bodies
                     Entity.Position @= (draggedPosition + mousePosition) / 2f
                     Entity.Size @= v3 (Vector3.Distance (draggedPosition, mousePosition)) 1f 0f
                     Entity.Rotation @= Quaternion.CreateLookAt2d (mousePosition - draggedPosition).V2
                     // Make line red which is applied on top of white
                     Entity.Color .= color 1f 0f 0f 1f
                     Entity.StaticImage .= Assets.Default.White
                     // Elevate the line above other entities (elevation 0) but below buttons (elevation 1)
                     Entity.Elevation .= 0.5f
                     // The line is not part of the physics simulation, so it has an empty body shape.
                     Entity.BodyShape .= EmptyShape] world |> ignore

            | None -> ()

        let color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f
        let boxNames = Array.init 32 (sprintf "Blobbo Contour %d")
        let boxCount = float32 boxNames.Length
        let boxSize = 8f
        // This scale is large enough such that the soft body doesn't form knots,
        // but small enough to not spawn individual boxes outside the border.
        // The body joints will pull individual boxes together no matter how far apart the boxes spawn.
        let spawnScale = boxSize * boxCount / 8f
        let spawnCenter = v3 0f 0f 0f
        
        // define center for stabilizing the contour shape and for mouse dragging
        let _ =
            World.doBall2d "Blobbo Center"
                [Entity.Position .= spawnCenter
                 Entity.Size .= v3Dup 16f
                 Entity.Visible .= false] world
        let center = world.DeclaredEntity
        
        for i in 0 .. Array.length boxNames - 1 do        // define soft body countour boxes
            // Arrange 32 points in a circle for soft body
            let boxAngle = MathF.Tau * float32 i / boxCount
            let x = cos boxAngle * spawnScale
            let y = sin boxAngle * spawnScale
            let (declaredBodyId, _) =
                World.doBox2d boxNames[i]
                    [Entity.Position .= spawnCenter + v3 x y 0f
                     Entity.Restitution .= 0.333f
                     Entity.Size .= v3 boxSize boxSize 0f
                     Entity.Substance .= Mass (0.1f / boxCount) // Make mass evenly distributed between the contour and the center
                     Entity.CollisionDetection .= Continuous
                     Entity.Color .= color] world
            // If the contour box is dragged directly, the many other joints counteract the mouse joint
            // and the soft body stays mid-air away from the mouse
            screen.MouseDragTarget.Map (Map.add world.DeclaredEntity center) world
            screen.SoftBodyContour.Map (Map.add declaredBodyId center) world
        
        // declare revolute joint linkage between contour boxes
        for (n1, n2) in Array.pairwise boxNames |> Array.add (Array.last boxNames, Array.head boxNames) do
            let _ =
                World.doBodyJoint2d $"{n1} Joint contour"
                    // Aside from using two entities directly, a relation of two entities in the same group can also be
                    // specified by starting with the parent link denoted by "^", then accessing the sub-entity using "/".
                    [Entity.BodyJointTarget .= Address.makeFromString $"^/{n1}"
                     Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/{n2}")
                     Entity.CollideConnected .= true // Each box linked should collide with each other
                     Entity.BreakingPoint .= infinityf
                     Entity.BodyJoint .= TwoBodyJoint2d
                        { CreateTwoBodyJoint = fun toPhysics _ a b ->
                            // Local coordinates are used here which centers at the body coordinates,
                            // but we still have to convert from world scale to physics engine scale ourselves.
                            let boxSize = toPhysics boxSize
                            RevoluteJoint (a, b, new _(0f, 0.5f * boxSize), new _(0f, -0.5f * boxSize), false) }] world |> ignore
            ()
        // declare distance joint linkage between contour boxes and center ball for stabilizing the shape
        for n in boxNames do
            let _ =
                World.doBodyJoint2d $"{n} Joint center"
                    // Aside from using two entities directly, a relation of two entities in the same group can also be
                    // specified by starting with the parent link denoted by "^", then accessing the sub-entity using "/".
                    [Entity.BodyJointTarget .= center.EntityAddress
                     Entity.BodyJointTarget2Opt .= Some (Address.makeFromString $"^/{n}")
                     Entity.BreakingPoint .= infinityf
                     Entity.BodyJoint .= TwoBodyJoint2d
                    { CreateTwoBodyJoint = fun toPhysics _ a b ->
                        // Local coordinates are used here which centers at the body coordinates,
                        // but we still have to convert from world scale to physics engine scale ourselves.
                        let boxSize = toPhysics boxSize
                        DistanceJoint (a, b, new _(0f, 0f), new _(0f, 0f), false, Length = toPhysics spawnScale + boxSize, DampingRatio = 1f, Frequency = 5f) }] world
            ()

        World.doEntity<RewindableDispatcher> "Box"
            [Entity.FacetNames .= Set.ofList [nameof StaticSpriteFacet]
             Entity.Position .= v3 0f 0f 0f
             Entity.Size .= v3Dup 16f
             Entity.BodyType .= Dynamic
             Entity.StaticImage .= Assets.Default.StaticSprite
             Entity.BodyShape .= BoxShape { Size = v3 1f 1f 0f; PropertiesOpt = None; TransformOpt = None }
             Entity.Substance .= Mass 1f
             Entity.CollisionDetection .= Continuous] world |> ignore

        if World.isKeyboardKeyPressed KeyboardKey.Space world then
            world.DeclaredEntity.SetRewindPreview world.GameTime world
        if World.isKeyboardKeyDown KeyboardKey.Space world then
            world.DeclaredEntity.RewindPreview.Map (fun r -> r - world.GameDelta - world.GameDelta) world
        let rewindPreview = world.DeclaredEntity.GetRewindPreview world
        if GameTime.notZero rewindPreview && World.isKeyboardKeyUp KeyboardKey.Space world then
            World.publish rewindPreview world.DeclaredEntity.RewindEvent world.DeclaredEntity world
            world.DeclaredEntity.SetRewindPreview GameTime.zero world
        World.endGroup world