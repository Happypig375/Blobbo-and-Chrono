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
    
    static let updateTessellation (entity : Entity) world =
        let eyeSocketTessellation =
            ContourTessellation.make
                (seq {
                  for direction in -1 .. 2 .. 1 do
                      MoveTo (v2 (single direction * 0.1f) 0.4f)
                      LineTo (v2 (single direction * 0.4f) 0.4f)
                      LineTo (v2 (single direction * 0.4f) -0.4f)
                      LineTo (v2 (single direction * 0.1f) -0.4f)
                      CloseContour })
                (ContourFill.ofColor Color.White)
                (ContourStroke.none)
                (entity.GetSize world * entity.GetScale world).V2
        entity.SetEyeSocketTessellation eyeSocketTessellation world
        Cascade

    static let getEyeTessellation (entity : Entity) world =
        let linearVelocity = (entity.GetLinearVelocity world).V2
        let eyeDirection =
            if linearVelocity.LengthSquared () > 0.0001f
            then Vector2.Normalize linearVelocity
            else v2Zero
        let eyeOffset = v2 (eyeDirection.X * 0.09f) (eyeDirection.Y * 0.29f)
        ContourTessellation.make
            (seq {
              for direction in -1 .. 2 .. 1 do
                  let center = v2 (single direction * 0.25f + eyeOffset.X) eyeOffset.Y
                  let halfExtent = 0.06f
                  MoveTo (v2 (center.X - halfExtent) (center.Y - halfExtent))
                  LineTo (v2 (center.X + halfExtent) (center.Y - halfExtent))
                  LineTo (v2 (center.X + halfExtent) (center.Y + halfExtent))
                  LineTo (v2 (center.X - halfExtent) (center.Y + halfExtent))
                  CloseContour })
            (ContourFill.ofColor Color.Black)
            (ContourStroke.none)
            (entity.GetSize world * entity.GetScale world).V2

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
         nonPersistent Entity.EyeSocketTessellation ContourTessellation.empty]

    override this.Register (entity, world) =
        for propertyName in
            [nameof Entity.Size; nameof Entity.Scale] do
            World.sense (constant $ updateTessellation entity) (entity.ChangeEvent propertyName) entity (nameof RectangleContour2dFacet) world
        updateTessellation entity world |> ignore<Handling>

    override this.Render (_, entity, world) = 
        let transform = entity.GetTransform world
        let clipOpt = entity.GetClipOpt world |> Option.toValueOption
        World.renderContour
            { Transform = transform
              ClipOpt = clipOpt
              Tessellation = entity.GetEyeSocketTessellation world } world
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

        World.doEntity<SquareDispatcher> "Square1"
            [Entity.Position |= v3 -100f 0f 0f
             Entity.Size .= v3Dup 64f
             Entity.LinearVelocity |= v3 5f 5f 0f] world |> ignore

        // declare quit button
        if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world then
            screen.SetGameplayState Quit world

        World.endGroup world