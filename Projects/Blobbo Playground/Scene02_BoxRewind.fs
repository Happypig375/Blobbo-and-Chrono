namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu
open BlobboPlayground

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type Scene02_BoxRewindDispatcher () =
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

        let (box, _) =
            World.doBox2d "Box"
                [Entity.Position |= v3 -90f 0f 0f
                 Entity.Size .= v3Dup 16f
                 Entity.LinearVelocity |= v3 100f 0f 0f
                 Entity.Friction .= 0f
                 Entity.FacetNames .= Set.ofList [nameof RewindableFacet]] world
            
        if screen.GetSelected world then
            World.setEye2dCenter v2Zero world
        
            if World.isKeyboardKeyPressed KeyboardKey.Space world then
                world.DeclaredEntity.SetRewindPreview (Some GameTime.zero) world
            if World.isKeyboardKeyDown KeyboardKey.Space world then
                world.DeclaredEntity.RewindPreview.Map (Option.map (fun r -> r + world.GameDelta + world.GameDelta)) world
            //match world.DeclaredEntity.GetRewindPreview world with
            //| Some rewindPreview when World.isKeyboardKeyUp KeyboardKey.Space world ->
            //    World.publish { RewindAnchorOpt = ValueNone; RewindTime = rewindPreview } world.DeclaredEntity.RewindEvent world.DeclaredEntity world
            //    world.DeclaredEntity.SetRewindPreview None world
            //| _ -> ()
        let (box2, _) =
            World.doBox2d "Box2"
                [Entity.Position |= v3 90f 0f 0f
                 Entity.Size .= v3Dup 16f
                 Entity.Friction .= 0f
                 Entity.FacetNames .= Set.ofList [nameof RewindableFacet]] world

        // declare quit button
        if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world then
            screen.SetGameplayState Quit world

        World.endGroup world