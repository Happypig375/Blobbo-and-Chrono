namespace BlobboPlayground
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module WaterContainerFacet =

    type Entity with
        member this.GetWaterContent world : single = this.Get (nameof this.WaterContent) world
        member this.SetWaterContent (value : single) world = this.Set (nameof this.WaterContent) value world
        member this.WaterContent = lens (nameof this.WaterContent) this this.GetWaterContent this.SetWaterContent
        member this.GetWorldFluidEmitter world : Entity Address = this.Get (nameof this.WorldFluidEmitter) world
        member this.SetWorldFluidEmitter (value : Entity Address) world = this.Set (nameof this.WorldFluidEmitter) value world
        member this.WorldFluidEmitter = lens (nameof this.WorldFluidEmitter) this this.GetWorldFluidEmitter this.SetWorldFluidEmitter
        member this.HeatEvent = stoa<unit> "Heat/Event" --> this

    /// Facet that gives an entity water container capabilities:
    /// tracks water content, connects to a fluid emitter, and responds to Heat events
    /// by converting stored water to smoke particles via the world fluid emitter.
    type WaterContainerFacet () =
        inherit Facet (false, false, false)

        let handleHeat (evt : Event<unit, Entity>) world =
            let entity = evt.Subscriber
            let waterContent = entity.GetWaterContent world
            if waterContent > 0.0f then
                match tryResolve (entity.GetWorldFluidEmitter world) entity with
                | Some emitter ->
                    let center =
                        let pos = entity.GetPosition world
                        let size = entity.GetSize world
                        pos + size * 0.5f
                    let smokeCount = int (waterContent * 32.0f) |> max 1
                    World.emitFluidParticles
                        (SArray.init smokeCount (fun _ ->
                            let jitter = v3 (Gen.randomf * 2.0f - 1.0f) (Gen.randomf * 2.0f - 1.0f) 0.0f * 12.0f
                            { FluidParticlePosition = center + jitter
                              FluidParticleVelocity = v3 (Gen.randomf - 0.5f) (Gen.randomf * -2.0f) 0.0f * 4.0f
                              FluidParticleConfig = "Smoke" }))
                        (emitter.GetFluidEmitterId world) world
                | None -> ()
                entity.SetWaterContent 0.0f world
            Cascade

        static member Properties =
            [define Entity.WaterContent 0.0f
             define Entity.WorldFluidEmitter Address.empty]

        override this.Register (entity, world) =
            World.sense handleHeat entity.HeatEvent entity (nameof WaterContainerFacet) world
