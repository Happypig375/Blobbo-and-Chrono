namespace BlobboPlayground
open System
open System.Collections.Generic
open System.Numerics
open Prime
open Nu

type RewindRecord = { PropertyName : string; PreviousValue : obj; TimePassed : GameTime }
type InteractionRecord = { WithOther : Entity Address; WorldTime : GameTime }
type Rewind = { RewindAnchorOpt : InteractionRecord voption; RewindTime : GameTime }
module Rewind =
    let [<Literal>] EventName = "Rewind/Event"
    let Event = stoa<Rewind> EventName
    let PreviewEvent = stoa<Rewind> "RewindPreview/Event"
module [<AutoOpen>] RewindableExtensions =
    type Entity with
        member this.GetRewindPreview world : GameTime option = this.Get (nameof this.RewindPreview) world
        member this.SetRewindPreview (value : GameTime option) world = this.Set (nameof this.RewindPreview) value world
        member this.RewindPreview = lens (nameof this.RewindPreview) this this.GetRewindPreview this.SetRewindPreview
        member this.GetRewindHistory world : RewindRecord list = this.Get (nameof this.RewindHistory) world
        member this.SetRewindHistory (value : RewindRecord list) world = this.Set (nameof this.RewindHistory) value world
        member this.RewindHistory = lens (nameof this.RewindHistory) this this.GetRewindHistory this.SetRewindHistory
        member this.GetTimeSinceLastHistoryEntry world : GameTime = this.Get (nameof this.TimeSinceLastHistoryEntry) world
        member this.SetTimeSinceLastHistoryEntry (value : GameTime) world = this.Set (nameof this.TimeSinceLastHistoryEntry) value world
        member this.TimeSinceLastHistoryEntry = lens (nameof this.TimeSinceLastHistoryEntry) this this.GetTimeSinceLastHistoryEntry this.SetTimeSinceLastHistoryEntry
        member this.GetRewindHistoryActiveInternal world : bool = this.Get (nameof this.RewindHistoryActiveInternal) world
        member this.SetRewindHistoryActiveInternal (value : bool) world = this.Set (nameof this.RewindHistoryActiveInternal) value world
        member this.RewindHistoryActiveInternal = lens (nameof this.RewindHistoryActiveInternal) this this.GetRewindHistoryActiveInternal this.SetRewindHistoryActiveInternal
        member this.RewindEvent = Rewind.Event --> this
        member this.RewindPreviewEvent = Rewind.PreviewEvent --> this

/// Register this after initialization
type RewindableFacet () =
    inherit Facet (false, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>]

    static member Properties =
        [define Entity.RewindPreview None
         define Entity.RewindHistory []
         define Entity.TimeSinceLastHistoryEntry GameTime.zero
         computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None // force body transform events to be published
         nonPersistent Entity.RewindHistoryActiveInternal true]

    override _.Register (entity, world) =
        // sense rewind event
        let (~-) = GameTime.unary ((~-) >> UpdateTime) ((~-) >> TickTime)
        World.sense (fun event world ->
            let entity = event.Subscriber
            let mutable remainingGameTime = event.Data.RewindTime
            let mutable rewindAnchorOpt = event.Data.RewindAnchorOpt
            if rewindAnchorOpt.IsNone &&
               (remainingGameTime <- remainingGameTime - entity.GetTimeSinceLastHistoryEntry world;
                remainingGameTime <= GameTime.zero)
            then entity.SetTimeSinceLastHistoryEntry -remainingGameTime world
            else
                let rewindProperties = Dictionary ()
                let mutable continued = true
                while continued do
                    match entity.GetRewindHistory world with
                    | rewindRecord :: rest ->
                        entity.SetRewindHistory rest world // NOTE: entity property must be updated before recursing via event publish
                        if rewindRecord.PropertyName <> Rewind.EventName then
                            rewindProperties[rewindRecord.PropertyName] <- rewindRecord.PreviousValue
                        else
                            let interactionRecord = rewindRecord.PreviousValue :?> InteractionRecord
                            if rewindAnchorOpt = ValueSome interactionRecord then
                                rewindAnchorOpt <- ValueNone
                            elif rewindAnchorOpt.IsNone then
                                World.publishUnsorted
                                    { RewindAnchorOpt = ValueSome { WithOther = entity.EntityAddress; WorldTime = interactionRecord.WorldTime }
                                      RewindTime = remainingGameTime }
                                    (acatf Rewind.Event interactionRecord.WithOther)
                                    entity
                                    world
                        if rewindAnchorOpt.IsNone then
                            remainingGameTime <- remainingGameTime - rewindRecord.TimePassed
                            if remainingGameTime <= GameTime.zero then
                                continued <- false
                                entity.SetTimeSinceLastHistoryEntry -remainingGameTime world
                    | [] -> continued <- false // TODO: rewinding past object spawning?
                entity.SetXtensionPropertyWithoutEvent (nameof Entity.AwakeTimeStamp) world.UpdateTime world // otherwise it would sleep
                entity.SetRewindHistoryActiveInternal false world
                for KeyValue (property, value) in rewindProperties do
                    entity.SetProperty property { entity.GetProperty property world with PropertyValue = value } world
                entity.SetRewindHistoryActiveInternal true world
            Cascade) entity.RewindEvent entity (nameof RewindableFacet) world

        // sense rewind preview event
        World.sense (fun event world ->
            let entity = event.Subscriber
            let mutable remainingGameTime = event.Data.RewindTime
            let mutable rewindAnchorOpt = event.Data.RewindAnchorOpt
            if rewindAnchorOpt.IsNone then remainingGameTime <- remainingGameTime - entity.GetTimeSinceLastHistoryEntry world
            if remainingGameTime > GameTime.zero then
                // render from history for the frame
                let mutable continued = true
                let mutable rewindHistory = entity.GetRewindHistory world
                let restoreTransform = entity.GetTransform world
                let restoreXtensions = System.Collections.Generic.Dictionary ()
                let mutable rewindTransform = restoreTransform
                while continued do
                    match rewindHistory with
                    | rewindRecord :: rest ->
                        rewindHistory <- rest
                        match rewindRecord.PropertyName with
                        // Intrinsic property list sync point 2/2
                        | nameof Entity.Position -> rewindTransform.Position <- unbox rewindRecord.PreviousValue
                        | nameof Entity.Rotation -> rewindTransform.Rotation <- unbox rewindRecord.PreviousValue
                        | nameof Entity.Size -> rewindTransform.Size <- unbox rewindRecord.PreviousValue
                        | nameof Entity.Scale -> rewindTransform.Scale <- unbox rewindRecord.PreviousValue
                        | nameof Entity.Elevation -> rewindTransform.Elevation <- unbox rewindRecord.PreviousValue
                        | Rewind.EventName ->
                            let interactionRecord = rewindRecord.PreviousValue :?> InteractionRecord
                            if rewindAnchorOpt = ValueSome interactionRecord then
                                rewindAnchorOpt <- ValueNone
                            elif rewindAnchorOpt.IsNone then
                                World.publishUnsorted
                                    { RewindAnchorOpt = ValueSome { WithOther = entity.EntityAddress; WorldTime = interactionRecord.WorldTime }
                                      RewindTime = remainingGameTime }
                                    (acatf Rewind.PreviewEvent interactionRecord.WithOther)
                                    entity
                                    world
                        | _ ->
                            if not (restoreXtensions.ContainsKey rewindRecord.PropertyName) then
                                restoreXtensions[rewindRecord.PropertyName] <- entity.GetProperty rewindRecord.PropertyName world
                            World.setEntityXtensionPropertyWithoutEvent rewindRecord.PropertyName
                                { restoreXtensions[rewindRecord.PropertyName] with PropertyValue = rewindRecord.PreviousValue }
                                entity world |> ignore<struct (bool * bool)>

                        if rewindAnchorOpt.IsNone && rewindRecord.TimePassed > GameTime.zero then // only render right before history has a time skip
                            remainingGameTime <- remainingGameTime - rewindRecord.TimePassed
                            continued <- remainingGameTime > GameTime.zero
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
            Cascade) entity.RewindEvent entity (nameof RewindableFacet) world
          
        // sense change events - property assignments
        let senseChangeEvent (changeProperty : Lens<'a, Entity>) =
            World.sense (fun event world ->
                let entity = event.Subscriber
                if entity.GetRewindHistoryActiveInternal world then
                    entity.RewindHistory.Map (List.cons { PropertyName = changeProperty.Name; PreviousValue = event.Data.Previous; TimePassed = entity.GetTimeSinceLastHistoryEntry world }) world
                    entity.SetTimeSinceLastHistoryEntry GameTime.zero world
                Cascade) changeProperty.ChangeEvent entity (nameof RewindableFacet) world
        senseChangeEvent entity.LinearVelocity
        senseChangeEvent entity.AngularVelocity
        // Intrinsic property list sync point 1/2
        senseChangeEvent entity.Position
        senseChangeEvent entity.Rotation
        senseChangeEvent entity.Size
        senseChangeEvent entity.Scale

        // record interactions
        World.sense (fun event world ->
            let entity = event.Subscriber
            let collidedWith = event.Data.BodyShapeSeparatee.BodyId.BodySource :?> Entity
            if collidedWith.Has<RewindableFacet> world then
                entity.RewindHistory.Map (List.cons
                    { PropertyName = Rewind.EventName
                      PreviousValue = { WithOther = collidedWith.EntityAddress
                                        WorldTime = world.GameTime }
                      TimePassed = entity.GetTimeSinceLastHistoryEntry world }) world
                entity.SetTimeSinceLastHistoryEntry GameTime.zero world
            Cascade) entity.BodySeparationExplicitEvent entity (nameof RewindableFacet) world

        // sense change events - body transform
        // NOTE: assumes BodyTransformEvent is fired at all, see the event firing criteria in WorldModule2.fs
        World.sense (fun event world ->
            let entity = event.Subscriber
            entity.RewindHistory.Map (fun rewindHistory ->
                [{ PropertyName = nameof Entity.Position; PreviousValue = entity.GetPosition world; TimePassed = GameTime.zero }
                 { PropertyName = nameof Entity.Rotation; PreviousValue = entity.GetRotation world; TimePassed = GameTime.zero }
                 { PropertyName = nameof Entity.LinearVelocity; PreviousValue = entity.GetLinearVelocity world; TimePassed = GameTime.zero }
                 { PropertyName = nameof Entity.AngularVelocity; PreviousValue = entity.GetAngularVelocity world; TimePassed = entity.GetTimeSinceLastHistoryEntry world }
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
            World.publish { RewindAnchorOpt = ValueNone; RewindTime = rewindPreview } entity.RewindPreviewEvent entity world
        | None -> ()