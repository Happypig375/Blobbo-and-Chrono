namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu
open BlobboPlayground

// this determines what state the game is in. To learn about ImSim in Nu, see -
// https://github.com/bryanedds/Nu/wiki/Immediate-Mode-for-Games-via-ImSim
type GameState =
    | Splash
    | Title
    | Scene01_Playground
    | Scene02_BoxRewind

// this extends the Game API to expose the above ImSim model as a property.
[<AutoOpen>]
module BlobboPlaygroundExtensions =
    type Game with
        member this.GetGameState world : GameState = this.Get (nameof Game.GameState) world
        member this.SetGameState (value : GameState) world = this.Set (nameof Game.GameState) value world
        member this.GameState = lens (nameof Game.GameState) this this.GetGameState this.SetGameState

// this is the dispatcher that customizes the top-level behavior of our game.
type BlobboPlaygroundDispatcher () =
    inherit GameDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Game.GameState Splash]

    // here we define the game's top-level behavior
    override this.Process (game, world) =

        // declare splash screen
        let behavior = Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)
        let results = World.beginScreen Simulants.Splash.Name (game.GetGameState world = Splash) behavior [] world
        if FQueue.contains Deselecting results && game.GetGameState world = Splash then game.SetGameState Title world
        World.endScreen world

        // declare title screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        World.beginScreen Simulants.Title.Name (game.GetGameState world = Title) behavior [] world |> ignore
        World.beginGroup "Gui" [] world
        World.beginPanel "Panel" [Entity.Size .= Constants.Render.DisplayVirtualResolution.V3; Entity.Layout .= Grid (v2i 8 8, Some FlowRightward, true)] world
        if World.doButton "Scene01_Playground" [Entity.Text .= "01"] world then game.SetGameState Scene01_Playground world
        if World.doButton "Scene02_BoxRewind" [Entity.Text .= "02"] world then game.SetGameState Scene02_BoxRewind world
        if World.doButton "Exit" [Entity.Text .= "Exit"] world && world.Unaccompanied then World.exit world
        World.endPanel world
        World.endGroup world
        World.endScreen world

        // declare scene 01
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let results = World.beginScreen<Scene01_PlaygroundDispatcher> Simulants.Scene01_Playground.Name (game.GetGameState world = Scene01_Playground) behavior [] world
        if FQueue.contains Select results then Simulants.Scene01_Playground.SetGameplayState Playing world
        if FQueue.contains Deselecting results then Simulants.Scene01_Playground.SetGameplayState Quit world
        if Simulants.Scene01_Playground.GetSelected world && Simulants.Scene01_Playground.GetGameplayState world = Quit then game.SetGameState Title world
        World.endScreen world

        // declare scene 02
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let results = World.beginScreen<Scene02_BoxRewindDispatcher> Simulants.Scene02_BoxRewind.Name (game.GetGameState world = Scene02_BoxRewind) behavior [] world
        if FQueue.contains Select results then Simulants.Scene02_BoxRewind.SetGameplayState Playing world
        if FQueue.contains Deselecting results then Simulants.Scene02_BoxRewind.SetGameplayState Quit world
        if Simulants.Scene02_BoxRewind.GetSelected world && Simulants.Scene02_BoxRewind.GetGameplayState world = Quit then game.SetGameState Title world
        World.endScreen world

        // handle Alt+F4 when not in editor
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world