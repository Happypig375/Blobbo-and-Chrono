namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

type Rewind =
    { GameTime : GameTime
      Position : Vector3
      Rotation : Quaternion
      Size : Vector3
      AngularVelocity : Vector3
      LinearVelocity : Vector3 }

module [<AutoOpen>] RewindableExtensions =
    type Entity with
        member this.GetRewindPreview world : GameTime option = this.Get (nameof this.RewindPreview) world
        member this.SetRewindPreview (value : GameTime option) world = this.Set (nameof this.RewindPreview) value world
        member this.RewindPreview = lens (nameof this.RewindPreview) this this.GetRewindPreview this.SetRewindPreview
        member this.GetBodyHistory world : FQueue<Rewind> = this.Get (nameof this.BodyHistory) world
        member this.SetBodyHistory (value : FQueue<Rewind>) world = this.Set (nameof this.BodyHistory) value world
        member this.BodyHistory = lens (nameof this.BodyHistory) this this.GetBodyHistory this.SetBodyHistory
        member this.RewindEvent = stoa<GameTime> "Rewind/Event" --> this

type RewindableFacet () =
    inherit Facet (false, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.TraversalHistoryMax (GameTime.ofSeconds 10)
         define Entity.RewindPreview None
         define Entity.BodyHistory FQueue.empty]

    override _.Register (entity, world) =
        World.sense (fun event world ->
            let entity = event.Subscriber
            let mutable found = false
            entity.BodyHistory.Map (FQueue.fold (fun newHistoryQueue rewindHistory ->
                if rewindHistory.GameTime >= event.Data then
                    if not found then
                        entity.SetPosition rewindHistory.Position world
                        entity.SetRotation rewindHistory.Rotation world
                        entity.SetAngularVelocity rewindHistory.AngularVelocity world
                        entity.SetLinearVelocity rewindHistory.LinearVelocity world
                        found <- true
                    newHistoryQueue
                else FQueue.conj rewindHistory newHistoryQueue
                ) FQueue.empty) world
            Cascade) entity.RewindEvent entity (nameof RewindableFacet) world

        World.sense (fun event world ->
            let entity : Entity = event.Subscriber
            let notRewinding = (entity.GetRewindPreview world).IsNone
            entity.SetBodyEnabled notRewinding world
            entity.SetPresence (if notRewinding then Exterior else Omnipresent) world
            Cascade
            ) entity.RewindPreview.ChangeEvent entity (nameof RewindableFacet) world

    override _.Render (_, entity, world) =
        match entity.GetRewindPreview world with
        | Some rewindPreview ->
            let mutable transform = entity.GetTransform world
            let color = entity.GetColor world
            let rewindColor = color.MapA ((*) 0.2f)

            // render from history for the frame
            for rewindHistory in entity.GetBodyHistory world do
                if rewindHistory.GameTime >= rewindPreview then
                    transform.Position <- rewindHistory.Position
                    transform.Rotation <- rewindHistory.Rotation
                    transform.Size <- rewindHistory.Size
                    transform.PresenceOverride <- ValueSome Omnipresent
                    entity.SetTransformByRefWithoutEvent (&transform, world)
                    entity.SetXtensionPropertyWithoutEvent "Color" rewindColor world
                    (entity.GetDispatcher world).Render (NormalPass, entity, world)
                    
            transform.PresenceOverride <- ValueNone
            entity.SetTransformByRefWithoutEvent (&transform, world)
            entity.SetXtensionPropertyWithoutEvent "Color" color world

        | None -> ()

    override _.Update (entity, world) =
        match entity.GetRewindPreview world with
        | Some _ ->
            entity.SetBodyEnabled false world // disable physics while previewing
        | None ->
            let historyMax = entity.GetTraversalHistoryMax world
            entity.BodyHistory.Map (fun history ->

                // discard oldest item if the second oldest can already represent the state at history maximum
                match history with
                | FQueue.Cons (_, FQueue.Cons (second, _) & newHistory) when second.GameTime <= world.GameTime - historyMax ->
                    newHistory
                | _ -> history

                // add current state
                |> FQueue.conj 
                    { GameTime = world.GameTime
                      Position = entity.GetPosition world
                      Rotation = entity.GetRotation world
                      Size = entity.GetSize world
                      AngularVelocity = entity.GetAngularVelocity world
                      LinearVelocity = entity.GetLinearVelocity world }) world

    override _.Edit (op, entity, world) =

        // ensure position history isn't stale when editing
        match op with
        | ViewportOverlay _ when world.Halted ->
            let transformHistory =
                FQueue.singleton
                    { GameTime = world.GameTime
                      Position = entity.GetPosition world
                      Rotation = entity.GetRotation world
                      Size = entity.GetSize world
                      AngularVelocity = entity.GetAngularVelocity world
                      LinearVelocity = entity.GetLinearVelocity world }
            entity.SetBodyHistory transformHistory world
        | _ -> ()