namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

type Rewind =
    { GameTime : GameTime
      // Marked as mutable for byref only! Do not mutate outside of that!
      mutable Transform : Transform
      AngularVelocity : Vector3
      LinearVelocity : Vector3 }

module [<AutoOpen>] RewindableExtensions =
    type Entity with
        member this.GetRewindPreview world : GameTime option = this.Get (nameof this.RewindPreview) world
        member this.SetRewindPreview (value : GameTime option) world = this.Set (nameof this.RewindPreview) value world
        member this.RewindPreview = lens (nameof this.RewindPreview) this this.GetRewindPreview this.SetRewindPreview
        member this.GetRewindHistory world : FQueue<Rewind> = this.Get (nameof this.RewindHistory) world
        member this.SetRewindHistory (value : FQueue<Rewind>) world = this.Set (nameof this.RewindHistory) value world
        member this.RewindHistory = lens (nameof this.RewindHistory) this this.GetRewindHistory this.SetRewindHistory
        member this.RewindEvent = stoa<GameTime> "Rewind/Event" --> this

type RewindableFacet () =
    inherit Facet (false, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.Presence Omnipresent // render the rewind preview even if offscreen
         define Entity.TraversalHistoryMax (GameTime.ofSeconds 10f)
         define Entity.RewindPreview None
         define Entity.RewindHistory FQueue.empty]

    override _.Register (entity, world) =
        World.sense (fun event world ->
            let entity = event.Subscriber
            let mutable found = false
            entity.RewindHistory.Map (FQueue.fold (fun newHistoryQueue rewindHistory ->
                if rewindHistory.GameTime >= event.Data then
                    if not found then
                        entity.SetTransformByRef (&rewindHistory.Transform, world) |> ignore
                        entity.SetAngularVelocity rewindHistory.AngularVelocity world |> ignore
                        entity.SetLinearVelocity rewindHistory.LinearVelocity world |> ignore
                        found <- true
                    newHistoryQueue
                else FQueue.conj rewindHistory newHistoryQueue
                ) FQueue.empty) world
            Resolve) entity.RewindEvent entity (nameof RewindableFacet) world

    override _.Render (_, entity, world) =
        match entity.GetRewindPreview world with
        | Some rewindPreview ->
            // render from history for the frame
            for rewindHistory in entity.GetRewindHistory world do
                if rewindHistory.GameTime >= rewindPreview then
                    let transform = &rewindHistory.Transform
                    let staticImage = entity.GetStaticImage world
                    let insetOpt = match entity.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
                    let clipOpt = entity.GetClipOpt world |> Option.toValueOption
                    let color = entity.GetColor world |> _.ScaleA(0.2f) // fade for preview
                    let blend = entity.GetBlend world
                    let emission = entity.GetEmission world
                    let flip = entity.GetFlip world

                    World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, staticImage, &transform, &insetOpt, &clipOpt, staticImage, &color, blend, &emission, flip, world)
        | None -> ()

    override _.Update (entity, world) =
        match entity.GetRewindPreview world with
        | Some _ ->
            entity.SetBodyEnabled false world // disable physics while previewing
        | None ->
            entity.SetBodyEnabled true world
            // process history for the frame
            let historyMax = entity.GetTraversalHistoryMax world
            entity.RewindHistory.Map (fun history ->
                if FQueue.notEmpty history then
                    let (head, tail) = FQueue.uncons history
                    if head.GameTime <= world.GameTime - historyMax then tail else history // OPTIMIZATION: only filter oldest item instead of all items.
                else history
                |> FQueue.conj 
                    { GameTime = world.GameTime
                      Transform = entity.GetTransform world
                      AngularVelocity = entity.GetAngularVelocity world
                      LinearVelocity = entity.GetLinearVelocity world }) world

    override _.Edit (op, entity, world) =

        // ensure position history isn't stale when editing
        match op with
        | ViewportOverlay _ when world.Halted ->
            let transformHistory =
                FQueue.singleton
                    { GameTime = world.GameTime
                      Transform = entity.GetTransform world
                      AngularVelocity = entity.GetAngularVelocity world
                      LinearVelocity = entity.GetLinearVelocity world }
            entity.SetRewindHistory transformHistory world
        | _ -> ()