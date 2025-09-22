namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

type RewindCommand =
    | RewindCommand of GameTime
    interface Command

type RewindableDispatcher () =
    inherit Entity2dDispatcher<unit, Message, RewindCommand> (true, false, false, ())

    static member Facets =
        [typeof<RigidBodyFacet>
         typeof<TraversalInterpolatedFacet>]

    static member Properties =
        [define Entity.TraversalHistoryMax (GameTime.ofSeconds 10f)]

    override _.Command ((), command, entity, world) =
        match command with
        | RewindCommand until ->
            let now = world.GameTime
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
                    newPositionHistory <- FQueue.conj (gameTime, position) newPositionHistory
                    newRotationHistory <- FQueue.conj (gameTime, rotation) newRotationHistory
                    newLinearVelocityHistory <- FQueue.conj (gameTime, linearVelocity) newLinearVelocityHistory
                    newAngularVelocityHistory <- FQueue.conj (gameTime, angularVelocity) newAngularVelocityHistory
                else
                    if not found
                    then
                        entity.SetPosition position world
                        entity.SetRotation rotation world
                        entity.SetLinearVelocity linearVelocity world
                        entity.SetAngularVelocity angularVelocity world
                        found <- true
                    else
                        let effect = World.createEntity<Effect2dDispatcher> DefaultOverlay None entity.Group world
                        effect.SetMountOpt None world // Don't let parent mounting override our position and rotation!
                        effect.SetPosition position world
                        effect.SetRotation rotation world
                        effect.SetSize (entity.GetSize world) world
                        effect.SetSelfDestruct true world // Calls World.destroyEntity for us.
                        let shadow a b = (a - b) / 3L
                        effect.SetEffectDescriptor
                            { EffectName = "RewindEffect"
                              LifeTimeOpt = GameTime.ap shadow shadow now gameTime |> Some
                              Definitions = Map.empty
                              Content =
                                Effects.StaticSprite (Effects.Resource (entity.GetStaticImage world).Pair,
                                    [|Effects.Color (color 1f 1f 1f 0.4f)|], Effects.Nil) }
                            world
            entity.SetPositionHistory newPositionHistory world
            entity.SetRotationHistory newRotationHistory world
            entity.SetLinearVelocityHistory newLinearVelocityHistory world
            entity.SetAngularVelocityHistory newAngularVelocityHistory world