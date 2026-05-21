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
        World.doBlockBody2d "Border"
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
            World.doBoxBody2d "Box"
                [Entity.Position |= v3 -90f 0f 0f
                 Entity.Size .= v3Dup 16f
                 Entity.LinearVelocity |= v3 100f 0f 0f
                 Entity.Friction .= 0f
                 Entity.FacetNames .= Set.ofList [nameof RewindableFacet]] world
            
        if screen.GetSelected world then
            World.setEye2dCenter v2Zero world
        
            if World.isKeyboardKeyPressed KeyboardKey.Down world then
                world.DeclaredEntity.SetRewindPreview (Some GameTime.zero) world
            if World.isKeyboardKeyDown KeyboardKey.Left world then
                world.DeclaredEntity.RewindPreview.Map (Option.map (fun r -> r + GameTime.ofUpdates 1)) world
            if World.isKeyboardKeyDown KeyboardKey.Right world then
                world.DeclaredEntity.RewindPreview.Map (Option.map (fun r -> r - GameTime.ofUpdates 1)) world
            if World.isKeyboardKeyPressed KeyboardKey.Space world then
                if world.Advancing then
                    World.setAdvancing false world
                    world.DeclaredEntity.SetRewindPreview (Some GameTime.zero) world
                else
                    World.setAdvancing true world
                    match world.DeclaredEntity.GetRewindPreview world with
                    | Some rewindPreview ->
                        World.publish { RewindAnchorOpt = ValueNone; RewindTime = rewindPreview } world.DeclaredEntity.RewindEvent world.DeclaredEntity world
                        world.DeclaredEntity.SetRewindPreview None world
                    | _ -> ()
            if not world.Advancing then
                World.doStaticSprite "Overlay" 
                    [Entity.Position .= v3 0f 0f 0.1f
                     Entity.Size .= Constants.Render.DisplayVirtualResolution.V3
                     Entity.StaticImage .= Assets.Default.White
                     Entity.Color .= color 0.5f 0.5f 0.5f 0.5f] world |> ignore
        let (box2, _) =
            World.doBoxBody2d "Box2"
                [Entity.Position |= v3 90f 0f 0f
                 Entity.Size .= v3Dup 16f
                 Entity.Friction .= 0f
                 Entity.FacetNames .= Set.ofList [nameof RewindableFacet]] world

        // declare quit button
        if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world then
            screen.SetGameplayState Quit world

        World.endGroup world