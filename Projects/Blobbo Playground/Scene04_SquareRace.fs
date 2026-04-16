namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu
open BlobboPlayground

[<AutoOpen>]
module SquareExtensions =
    type Entity with
        member this.GetEyeSocketTessellation world : ContourTessellation = this.Get (nameof Entity.EyeSocketTessellation) world
        member this.SetEyeSocketTessellation (value : ContourTessellation) world = this.Set (nameof Entity.EyeSocketTessellation) value world
        member this.EyeSocketTessellation = lens (nameof Entity.EyeSocketTessellation) this this.GetEyeSocketTessellation this.SetEyeSocketTessellation

type SquareDispatcher () =
    inherit Entity2dDispatcher (true, false, false)

    static let eyeSocketLeftX = 0.05f
    static let eyeSocketRightX = 0.45f
    static let eyeSocketTopY = 0.25f
    static let eyeSocketBottomY = -0.25f
    static let eyeSocketCenterX = (eyeSocketLeftX + eyeSocketRightX) * 0.5f
    static let eyeDirections = [-1; 1]
    
    static let updateEyeSocketTessellation (entity : Entity) world =
        let eyeSocketTessellation =
            ContourTessellation.make
                (seq {
                    for direction in eyeDirections do
                        MoveTo (v2 (single direction * eyeSocketLeftX) eyeSocketTopY)
                        LineTo (v2 (single direction * eyeSocketRightX) eyeSocketTopY)
                        LineTo (v2 (single direction * eyeSocketRightX) eyeSocketBottomY)
                        LineTo (v2 (single direction * eyeSocketLeftX) eyeSocketBottomY)
                        CloseContour })
                (ContourFill.ofColor Color.White)
                (ContourStroke.antiAliased Color.Black 1f)
                (entity.GetSize world * entity.GetScale world).V2
        entity.SetEyeSocketTessellation eyeSocketTessellation world
        Cascade

    static let getEyeTessellation (entity : Entity) world =
        let linearVelocity = (entity.GetLinearVelocity world).V2
        let eyeDirection =
            if linearVelocity.LengthSquared () > 0.0001f
            then Vector2.Normalize linearVelocity
            else v2Zero
        let clamp value minValue maxValue = value |> max minValue |> min maxValue
        let eyeOffset = v2 (eyeDirection.X * 0.12f) (eyeDirection.Y * 0.18f)
        ContourTessellation.make
            (seq {
              for direction in eyeDirections do
                  let socketX1 = single direction * eyeSocketLeftX
                  let socketX2 = single direction * eyeSocketRightX
                  let socketMinX = min socketX1 socketX2
                  let socketMaxX = max socketX1 socketX2
                  let socketMinY = eyeSocketBottomY
                  let socketMaxY = eyeSocketTopY
                  let margin = 0.00f
                  let halfExtent = 0.15f
                  let unclampedCenter = v2 (single direction * eyeSocketCenterX + eyeOffset.X) eyeOffset.Y
                  let center =
                      v2
                          (clamp unclampedCenter.X (socketMinX + halfExtent + margin) (socketMaxX - halfExtent - margin))
                          (clamp unclampedCenter.Y (socketMinY + halfExtent + margin) (socketMaxY - halfExtent - margin))
                  MoveTo (v2 (center.X - halfExtent) (center.Y - halfExtent))
                  LineTo (v2 (center.X + halfExtent) (center.Y - halfExtent))
                  LineTo (v2 (center.X + halfExtent) (center.Y + halfExtent))
                  LineTo (v2 (center.X - halfExtent) (center.Y + halfExtent))
                  CloseContour })
            (ContourFill.ofColor Color.Black)
            (ContourStroke.none)
            (entity.GetSize world * entity.GetScale world).V2

    static let trailEffect color =
        { Effects.EffectName = "Trail"
          Effects.LifeTimeOpt = Some (GameTime.ofSeconds 1.0)
          Effects.Definitions = Map.empty
          Effects.Content =
            Effects.StaticSprite (Effects.Resource (AssetTag.toPair Assets.Default.White),
                [|Effects.Sizes (Effects.Scalar, Effects.Linear, Once,
                    [|{ TweenValue = v3One; TweenLength = GameTime.ofSeconds 1.0 }
                      { TweenValue = v3Zero; TweenLength = GameTime.zero }|])
                  Effects.Color color|], Effects.Nil) }

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<RectangleContour2dFacet>]

    static member Properties =
        [define Entity.MountOpt None
         define Entity.BodyType Dynamic
         define Entity.BodyShape (BoxShape { Size = v3One; TransformOpt = None; PropertiesOpt = None })
         define Entity.FillColor Color.Green
         define Entity.StrokeColor Color.Black
         define Entity.StrokeThickness 2.0f
         define Entity.Gravity GravityIgnore
         define Entity.AngularFactor v3Zero // No rotation
         define Entity.Friction 0f
         define Entity.Restitution 1f // Perfectly elastic collisions
         nonPersistent Entity.EyeSocketTessellation ContourTessellation.empty]

    override this.Register (entity, world) =
        for propertyName in
            [nameof Entity.Size; nameof Entity.Scale] do
            World.sense (constant $ updateEyeSocketTessellation entity) (entity.ChangeEvent propertyName) entity (nameof RectangleContour2dFacet) world
        updateEyeSocketTessellation entity world |> ignore<Handling>

    override this.Update (entity, world) =
        let effect = World.createEntity<Effect2dDispatcher> (Some Address.parent) DefaultOverlay None entity.Group world
        effect.SetTransform (entity.GetTransform world) world
        effect.SetElevation (entity.GetElevation world - 0.01f) world
        effect.SetSelfDestruct true world
        effect.SetEffectDescriptor (trailEffect (entity.GetFillColor world)) world

    override this.Render (_, entity, world) = 
        let mutable transform = entity.GetTransform world
        let clipOpt = entity.GetClipOpt world |> Option.toValueOption
        transform.Elevation <- Single.BitIncrement transform.Elevation
        World.renderContour
            { Transform = transform
              ClipOpt = clipOpt
              Tessellation = entity.GetEyeSocketTessellation world } world
        transform.Elevation <- Single.BitIncrement transform.Elevation
        World.renderContour
            { Transform = transform
              ClipOpt = clipOpt
              Tessellation = getEyeTessellation entity world } world

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type Scene04_SquareRaceDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit]

    // here we define the behavior of our gameplay
    override this.Process (selectionResults, screen, world) =

        World.beginGroup "Group" [] world
        // declare border
        World.doBlock2d "Border"
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

        World.doEntity<SquareDispatcher> "Green"
            [Entity.Position |= v3 -100f 0f 0f
             Entity.LinearVelocity |= v3 32f 32f 0f] world |> ignore
        World.doEntity<SquareDispatcher> "Red"
            [Entity.Position |= v3 -132f 0f 0f
             Entity.LinearVelocity |= v3 -32f 32f 0f
             Entity.FillColor .= Color.Red] world |> ignore
        World.doEntity<SquareDispatcher> "Yellow"
            [Entity.Position |= v3 -100f -32f 0f
             Entity.LinearVelocity |= v3 32f -32f 0f
             Entity.FillColor .= Color.Yellow] world |> ignore
        World.doEntity<SquareDispatcher> "Blue"
            [Entity.Position |= v3 -132f -32f 0f
             Entity.LinearVelocity |= v3 -32f -32f 0f
             Entity.FillColor .= Color.Blue] world |> ignore

        // declare quit button
        if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world then
            screen.SetGameplayState Quit world

        World.endGroup world