namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

module [<AutoOpen>] RewindableExtensions =
    type Entity with
        member this.GetRewindPreview world : GameTime = this.Get (nameof this.RewindPreview) world
        member this.SetRewindPreview (value : GameTime) world = this.Set (nameof this.RewindPreview) value world
        member this.RewindPreview = lens (nameof this.RewindPreview) this this.GetRewindPreview this.SetRewindPreview
        member this.RewindEvent = stoa<GameTime> "Rewind/Event" --> this

type RewindableDispatcher () =
    inherit Entity2dDispatcherImSim (true, false, false)

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<TraversalInterpolatedFacet>]

    static member Properties =
        [define Entity.TraversalHistoryMax (GameTime.ofSeconds 10f)
         define Entity.RewindPreview GameTime.zero]

    override _.Process (entity, world) =
        let (previewing, rewinding, until) =
            let rewindPreview = entity.GetRewindPreview world
            match World.doSubscription "RewindSubscription" entity.RewindEvent world with
            | FQueue.Cons (rewind, _) -> (GameTime.notZero rewindPreview, true, rewind)
            | FQueue.Nil -> (GameTime.notZero rewindPreview, false, rewindPreview)
        entity.SetBodyEnabled (not previewing) world
        let positionHistory = entity.GetPositionHistory world
        let rotationHistory = entity.GetRotationHistory world
        let linearVelocityHistory = entity.GetLinearVelocityHistory world
        let angularVelocityHistory = entity.GetAngularVelocityHistory world
        let mutable positionHistoryEnumerator = (FQueue.toSeq positionHistory).GetEnumerator ()
        let mutable rotationHistoryEnumerator = (FQueue.toSeq rotationHistory).GetEnumerator ()
        let mutable linearVelocityHistoryEnumerator = (FQueue.toSeq linearVelocityHistory).GetEnumerator ()
        let mutable angularVelocityHistoryEnumerator = (FQueue.toSeq angularVelocityHistory).GetEnumerator ()
        let mutable newPositionHistory = FQueue.empty
        let mutable newRotationHistory = FQueue.empty
        let mutable newLinearVelocityHistory = FQueue.empty
        let mutable newAngularVelocityHistory = FQueue.empty
        let mutable found = false
        while positionHistoryEnumerator.MoveNext () &&
              rotationHistoryEnumerator.MoveNext () &&
              linearVelocityHistoryEnumerator.MoveNext () &&
              angularVelocityHistoryEnumerator.MoveNext () do
            let (gameTime, position) = positionHistoryEnumerator.Current
            let (rotationGameTime, rotation) = rotationHistoryEnumerator.Current
            let (linearVelocityGameTime, linearVelocity) = linearVelocityHistoryEnumerator.Current
            let (angularVelocityGameTime, angularVelocity) = angularVelocityHistoryEnumerator.Current
            assert (gameTime = rotationGameTime &&
                    gameTime = linearVelocityGameTime &&
                    gameTime = angularVelocityGameTime)
            if gameTime < until then
                if rewinding then
                    newPositionHistory <- FQueue.conj (gameTime, position) newPositionHistory
                    newRotationHistory <- FQueue.conj (gameTime, rotation) newRotationHistory
                    newLinearVelocityHistory <- FQueue.conj (gameTime, linearVelocity) newLinearVelocityHistory
                    newAngularVelocityHistory <- FQueue.conj (gameTime, angularVelocity) newAngularVelocityHistory
            else
                if not found
                then
                    if rewinding then
                        entity.SetPosition position world
                        entity.SetRotation rotation world
                        entity.SetLinearVelocity linearVelocity world
                        entity.SetAngularVelocity angularVelocity world
                    found <- true
                elif previewing then
                    World.doEffect2d $"{gameTime.Seconds}"
                        [Entity.MountOpt .= None // Don't let parent mounting override our position and rotation!
                         Entity.Position .= position
                         Entity.Rotation .= rotation
                         Entity.Size .= entity.GetSize world
                         Entity.EffectDescriptor .=
                            { EffectName = "RewindEffect"
                              LifeTimeOpt = None
                              Definitions = Map.empty
                              Content =
                                Effects.StaticSprite (Effects.Resource (entity.GetStaticImage world).Pair,
                                    [|Effects.Color (color 1f 1f 1f 0.4f)|], Effects.Nil) }
                        ] world
        if rewinding then
            entity.SetPositionHistory newPositionHistory world
            entity.SetRotationHistory newRotationHistory world
            entity.SetLinearVelocityHistory newLinearVelocityHistory world
            entity.SetAngularVelocityHistory newAngularVelocityHistory world