namespace BlobboAndChrono
open System
open System.Numerics
open Prime
open Nu
open BlobboAndChrono

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit]

    // here we define the behavior of our gameplay
    override this.Process (_, screen, world) =

        // Keep the external browser/audio side responsive without blocking Nu's frame loop.
        CompositionRoot.Current |> Option.iter (fun root -> root.Pump ())

        // begin scene declaration
        World.beginGroupFromFile "Scene" "Assets/Gameplay/Scene.nugroup" [] world

        // declare static model
        let rotation = Quaternion.CreateFromAxisAngle ((v3 1.0f 0.75f 0.5f).Normalized, world.UpdateTime % 360L |> single |> Math.DegreesToRadiansF)
        World.doStaticModel "StaticModel" [Entity.Scale .= v3Dup 0.5f; Entity.Rotation @= rotation] world

        // Minimal runnable status/obstacle visualization until a platform renderer is wired.
        match CompositionRoot.Current with
        | Some root ->
            let snapshot = root.Simulation.Snapshot
            let browser = root.LatestBrowserEvent |> Option.map string |> Option.defaultValue "none"
            World.doButton "AudioStatus" [Entity.Position .= v3 -220.0f 150.0f 0.0f; Entity.Text .= sprintf "Audio %d Obstacles %d Browser %s" snapshot.SourceSampleClock snapshot.Obstacles.Length browser] world |> ignore
        | None -> ()

        // declare quit button
        if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world then
            screen.SetGameplayState Quit world

        // ensure game is unpaused when quitting
        if screen.GetGameplayState world = Quit then
            World.setTimeAdvancing true world

        // end scene declaration
        World.endGroup world
