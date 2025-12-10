namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

module [<AutoOpen>] RewindableExtensions =
    type Entity with
        member this.GetRewindPreview world : GameTime option = this.Get (nameof this.RewindPreview) world
        member this.SetRewindPreview (value : GameTime option) world = this.Set (nameof this.RewindPreview) value world
        member this.RewindPreview = lens (nameof this.RewindPreview) this this.GetRewindPreview this.SetRewindPreview
        member this.GetRewindHistory world : (string * obj * GameTime) list = this.Get (nameof this.RewindHistory) world
        member this.SetRewindHistory (value : (string * obj * GameTime) list) world = this.Set (nameof this.RewindHistory) value world
        member this.RewindHistory = lens (nameof this.RewindHistory) this this.GetRewindHistory this.SetRewindHistory
        member this.GetRewindHistoryTimeStamp world : GameTime = this.Get (nameof this.RewindHistoryTimeStamp) world
        member this.SetRewindHistoryTimeStamp (value : GameTime) world = this.Set (nameof this.RewindHistoryTimeStamp) value world
        member this.RewindHistoryTimeStamp = lens (nameof this.RewindHistoryTimeStamp) this this.GetRewindHistoryTimeStamp this.SetRewindHistoryTimeStamp
        member this.GetTimeSinceLastHistoryEntry world : GameTime = this.Get (nameof this.TimeSinceLastHistoryEntry) world
        member this.SetTimeSinceLastHistoryEntry (value : GameTime) world = this.Set (nameof this.TimeSinceLastHistoryEntry) value world
        member this.TimeSinceLastHistoryEntry = lens (nameof this.TimeSinceLastHistoryEntry) this this.GetTimeSinceLastHistoryEntry this.SetTimeSinceLastHistoryEntry
        member this.GetRewindHistoryActive world : bool = this.Get (nameof this.RewindHistoryActive) world
        member this.SetRewindHistoryActive (value : bool) world = this.Set (nameof this.RewindHistoryActive) value world
        member this.RewindHistoryActive = lens (nameof this.RewindHistoryActive) this this.GetRewindHistoryActive this.SetRewindHistoryActive
        member this.RewindEvent = stoa<GameTime> "Rewind/Event" --> this // relative time to rewind from now

/// Register this after initialization
type RewindableFacet () =
    inherit Facet (false, false, false)

    static let rewindPreviewDuration = GameTime.ofSeconds 1.0
    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.TraversalHistoryMax (GameTime.ofSeconds 10)
         define Entity.RewindPreview None
         define Entity.RewindHistory []
         define Entity.TimeSinceLastHistoryEntry GameTime.zero
         computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None // force body transform events to be published
         nonPersistent Entity.RewindHistoryTimeStamp GameTime.zero
         nonPersistent Entity.RewindHistoryActive true]

    override _.Register (entity, world) =

        // sense rewind event
        let (~-) = GameTime.unary ((~-) >> UpdateTime) ((~-) >> TickTime)
        World.sense (fun event world ->
            let entity = event.Subscriber
            let mutable remainingGameTime = event.Data - entity.GetTimeSinceLastHistoryEntry world
            if remainingGameTime <= GameTime.zero
            then entity.SetTimeSinceLastHistoryEntry -remainingGameTime world
            else entity.RewindHistory.Map (fun rewindHistory ->
                let mutable rewindHistory = rewindHistory
                let rewindProperties = System.Collections.Generic.Dictionary ()
                let mutable continued = true
                while continued do
                    match rewindHistory with
                    | (property, previousValue, delay) :: rest ->
                        remainingGameTime <- remainingGameTime - delay
                        rewindHistory <- rest
                        rewindProperties[property] <- previousValue
                        if remainingGameTime <= GameTime.zero then
                            continued <- false
                            entity.SetTimeSinceLastHistoryEntry -remainingGameTime world
                    | [] -> continued <- false // TODO: rewinding past object spawning?
                entity.SetXtensionPropertyWithoutEvent (nameof Entity.AwakeTimeStamp) world.UpdateTime world // otherwise it would sleep
                entity.SetRewindHistoryActive false world
                for KeyValue (property, value) in rewindProperties do
                    entity.SetProperty property { entity.GetProperty property world with PropertyValue = value } world
                entity.SetRewindHistoryActive true world
                rewindHistory) world
            Cascade) entity.RewindEvent entity (nameof RewindableFacet) world
          
        // sense change events - property assignments
        let senseChangeEvent (changeProperty : Lens<'a, Entity>) =
            World.sense (fun event world ->
                let entity = event.Subscriber
                if entity.GetRewindHistoryActive world then
                    entity.RewindHistory.Map (List.cons (changeProperty.Name, event.Data.Previous, entity.GetTimeSinceLastHistoryEntry world)) world
                    entity.SetTimeSinceLastHistoryEntry GameTime.zero world
                Cascade) changeProperty.ChangeEvent entity (nameof RewindableFacet) world
        senseChangeEvent entity.LinearVelocity
        senseChangeEvent entity.AngularVelocity
        // Intrinsic property list sync point 1/2
        senseChangeEvent entity.Position
        senseChangeEvent entity.Rotation
        senseChangeEvent entity.Size
        senseChangeEvent entity.Scale

        // sense change events - body transform
        // NOTE: assumes BodyTransformEvent is fired at all, see the event firing criteria in WorldModule2.fs
        World.sense (fun event world ->
            let entity = event.Subscriber
            entity.RewindHistory.Map (fun rewindHistory ->
                [(nameof Entity.Position, entity.GetPosition world, GameTime.zero)
                 (nameof Entity.Rotation, entity.GetRotation world, GameTime.zero)
                 (nameof Entity.LinearVelocity, entity.GetLinearVelocity world, GameTime.zero)
                 (nameof Entity.AngularVelocity, entity.GetAngularVelocity world, entity.GetTimeSinceLastHistoryEntry world)
                 yield! rewindHistory]) world
            entity.SetTimeSinceLastHistoryEntry GameTime.zero world
            entity.Physics event.Data.BodyCenter event.Data.BodyRotation event.Data.BodyLinearVelocity event.Data.BodyAngularVelocity world
            Cascade) entity.BodyTransformEvent entity (nameof RewindableFacet) world
            
        // stop physics and make invisible during rewind preview
        World.sense (fun event world ->
            let entity : Entity = event.Subscriber
            let notRewinding = event.Data.Value :?> GameTime option |> Option.isNone
            entity.SetBodyEnabled notRewinding world
            entity.SetPresence (if notRewinding then Exterior else Omnipresent) world
            Cascade) entity.RewindPreview.ChangeEvent entity (nameof RewindableFacet) world

        // increment time since last history entry (use group Update because entity Update only runs when entity is on screen - we don't skip rewind steps when entity is off screen!)
        World.sense (fun event world ->
            let entity : Entity = event.Subscriber
            if (entity.GetRewindPreview world).IsNone then
                entity.TimeSinceLastHistoryEntry.Map ((+) world.GameDelta) world
            Cascade) entity.Group.UpdateEvent entity (nameof RewindableFacet) world

    override _.Render (_, entity, world) =
        match entity.GetRewindPreview world with
        | Some rewindPreview ->
            let mutable remainingGameTime = rewindPreview - entity.GetTimeSinceLastHistoryEntry world
            if remainingGameTime > GameTime.zero then
                // render from history for the frame
                let mutable continued = true
                let mutable rewindHistory = entity.GetRewindHistory world
                let restoreTransform = entity.GetTransform world
                let restoreXtensions = System.Collections.Generic.Dictionary ()
                let mutable rewindTransform = restoreTransform
                while continued do
                    match rewindHistory with
                    | (property, previous, delay) :: rest ->
                        remainingGameTime <- remainingGameTime - delay
                        rewindHistory <- rest
                        if true then //if remainingGameTime <= rewindPreviewDuration then
                            continued <- remainingGameTime > GameTime.zero
                            match property with
                            // Intrinsic property list sync point 2/2
                            | nameof Entity.Position -> rewindTransform.Position <- unbox previous
                            | nameof Entity.Rotation -> rewindTransform.Rotation <- unbox previous
                            | nameof Entity.Size -> rewindTransform.Size <- unbox previous
                            | nameof Entity.Scale -> rewindTransform.Scale <- unbox previous
                            | nameof Entity.Elevation -> rewindTransform.Elevation <- unbox previous
                            | _ ->
                                if not (restoreXtensions.ContainsKey property) then restoreXtensions[property] <- entity.GetProperty property world
                                World.setEntityXtensionPropertyWithoutEvent property { restoreXtensions[property] with PropertyValue = previous } entity world |> ignore<struct (bool * bool)>
                            entity.SetTransformByRefWithoutEvent (&rewindTransform, world)
                            let color = entity.GetColor world
                            entity.SetXtensionPropertyWithoutEvent (nameof Entity.Color) (color.MapA ((*) 0.2f)) world

                            // World.renderEntity using NormalPass but omit this facet
                            for facet in entity.GetFacets world do
                                if facet.GetType () <> typeof<RewindableFacet> then
                                    facet.Render (NormalPass, entity, world)
                            (entity.GetDispatcher world).Render (NormalPass, entity, world)

                            entity.SetXtensionPropertyWithoutEvent (nameof Entity.Color) color world
                    | [] -> continued <- false // TODO: rewinding past object spawning?
                for KeyValue (name, xtension) in restoreXtensions do
                    World.setEntityXtensionPropertyWithoutEvent name xtension entity world |> ignore<struct (bool * bool)>
                entity.SetTransformByRefWithoutEvent (&restoreTransform, world)
        | None -> ()