namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

type RewindCommand =
    | RewindCommand of int
    interface Command

type RewindableDispatcher () =
    inherit Entity2dDispatcher<unit, Message, RewindCommand> (true, false, false, ())

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<TraversalInterpolatedFacet>]

    static member Properties =
        [define Entity.TraversalHistoryMax 600]

    override _.Command ((), command, entity, world) =
        match command with
        | RewindCommand frames ->
            let mutable i = 0 // Temporary, until GameTime is supported in TraversalInterpolatedFacet properly
            entity.AngularVelocityHistory.Map (fun history ->
                FQueue.foldBack (fun v list ->
                    i <- i + 1
                    if i < frames then list
                    elif i = frames then
                        entity.SetAngularVelocity v world
                        list
                    else v :: list) history []
                |> List.rev
                |> FQueue.ofList
                ) world
            let mutable i = 0 // Temporary, until GameTime is supported in TraversalInterpolatedFacet properly
            entity.LinearVelocityHistory.Map (fun history ->
                FQueue.foldBack (fun v list ->
                    i <- i + 1
                    if i < frames then list
                    elif i = frames then
                        entity.SetLinearVelocity v world
                        list
                    else v :: list) history []
                |> List.rev
                |> FQueue.ofList
                ) world
            let mutable i = 0 // Temporary, until GameTime is supported in TraversalInterpolatedFacet properly
            entity.PositionHistory.Map (fun history ->
                FQueue.foldBack (fun v list ->
                    i <- i + 1
                    if i < frames then list
                    elif i = frames then
                        entity.SetPosition v world
                        list
                    else v :: list) history []
                |> List.rev
                |> FQueue.ofList
                ) world
            let mutable i = 0 // Temporary, until GameTime is supported in TraversalInterpolatedFacet properly
            entity.RotationHistory.Map (fun history ->
                FQueue.foldBack (fun v list ->
                    i <- i + 1
                    if i < frames then list
                    elif i = frames then
                        entity.SetRotation v world
                        list
                    else v :: list) history []
                |> List.rev
                |> FQueue.ofList
                ) world
            ()