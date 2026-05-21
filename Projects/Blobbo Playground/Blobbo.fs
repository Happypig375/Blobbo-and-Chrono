namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

module [<AutoOpen>] BlobboExtensions =
    type Entity with
        member this.GetWorldFluidEmitter world : Entity Address = this.Get (nameof this.WorldFluidEmitter) world
        member this.SetWorldFluidEmitter (value : Entity Address) world = this.Set (nameof this.WorldFluidEmitter) value world
        member this.WorldFluidEmitter = lens (nameof this.WorldFluidEmitter) this this.GetWorldFluidEmitter this.SetWorldFluidEmitter
        member this.GetAbsorbingWater world : bool = this.Get (nameof this.AbsorbingWater) world
        member this.SetAbsorbingWater (value : bool) world = this.Set (nameof this.AbsorbingWater) value world
        member this.AbsorbingWater = lens (nameof this.AbsorbingWater) this this.GetAbsorbingWater this.SetAbsorbingWater
        member this.GetBlobboCenter world : Vector3 = this.Get (nameof this.BlobboCenter) world
        member this.SetBlobboCenter (value : Vector3) world = this.Set (nameof this.BlobboCenter) value world
        member this.BlobboCenter = lens (nameof this.BlobboCenter) this this.GetBlobboCenter this.SetBlobboCenter
        member this.GetBlobboContour world : struct (Vector3 * Vector3) SArray = this.Get (nameof this.BlobboContour) world
        member this.SetBlobboContour (value : struct (Vector3 * Vector3) SArray) world = this.Set (nameof this.BlobboContour) value world
        member this.BlobboContour = lens (nameof this.BlobboContour) this this.GetBlobboContour this.SetBlobboContour
        member this.ShootEvent = stoa<Vector3> "Shoot/Event" --> this

type BlobboDispatcher () =
    inherit FluidEmitter2dDispatcher ()

    static let minBlobboSize = 3

    static member Properties =
        [define Entity.Size (v3 60f 60f 0f)
         define Entity.WorldFluidEmitter Address.empty
         ]

    override _.Register (blobbo, world) =
        base.Register (blobbo, world)

        let eject () =
            World.monitor (fun event world ->
                // eject out of bounds particles to the world fluid emitter
                let blobbo : Entity = event.Subscriber
                let emitter = tryResolve (blobbo.GetWorldFluidEmitter world) blobbo |> Option.get
                World.emitFluidParticles (event.Data.OutOfBoundsParticles |> SArray.map (fun p -> { p with FluidParticleConfig = "Water" })) (emitter.GetFluidEmitterId world) world
                Cascade) blobbo.FluidEmitterUpdateEvent blobbo world

        let shoot () =
            // shoot particles when at least 3 are in body
            World.monitor (fun event world ->
                let blobbo : Entity = event.Subscriber
                let mutable i = 0
                let mutable shoot = ValueNone
                World.chooseFluidParticles (fun p ->
                    i <- inc i
                    if i = minBlobboSize then
                        shoot <- ValueSome { p with FluidParticleVelocity = p.FluidParticleVelocity + (event.Data - p.FluidParticlePosition) * 2f; FluidParticleConfig = "Water" }
                        ValueNone
                    else ValueSome p) (blobbo.GetFluidEmitterId world) world
                match shoot with
                | ValueSome p ->
                    let emitter = tryResolve (blobbo.GetWorldFluidEmitter world) blobbo |> Option.get
                    World.emitFluidParticles (SArray.singleton p) (emitter.GetFluidEmitterId world) world
                | ValueNone -> ()
                Cascade) blobbo.ShootEvent blobbo world

        ()

    override _.Update (blobbo, world) =
        let mutable newPosition = v3Zero
        let mutable particleCount = 0

        // update center to be average of particle positions
        if particleCount > 0 then blobbo.SetPosition (newPosition / single particleCount) world

        if blobbo.GetAbsorbingWater world then
            // when expanding, convert world fluid emitter particles to blobbo particles
            let bounds = blobbo.GetBounds world
            let maxParticles = 10
            let absorbed = ResizeArray maxParticles
            match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
            | Some (emitter : Entity) ->
                World.chooseFluidParticles (fun p ->
                    if bounds.Contains p.FluidParticlePosition <> ContainmentType.Disjoint && absorbed.Count < maxParticles then
                        absorbed.Add p
                        ValueNone
                    else ValueSome p) (emitter.GetFluidEmitterId world) world
                World.emitFluidParticles (SArray.init absorbed.Count (fun i -> { absorbed[i] with FluidParticleConfig = "Oil" })) (blobbo.GetFluidEmitterId world) world
            | None -> ()

        let loseWater () =
            // when contracting, convert blobbo particles to world fluid emitter particles
            let mutable i = 0
            let bounds = blobbo.GetBounds world
            let maxParticles = 10
            let absorbed = ResizeArray maxParticles
            match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
            | Some (emitter : Entity) ->
                World.chooseFluidParticles (fun p ->
                    i <- inc i
                    if i >= minBlobboSize && bounds.Contains p.FluidParticlePosition <> ContainmentType.Disjoint && absorbed.Count < maxParticles then
                        absorbed.Add p
                        ValueNone
                    else ValueSome p) (blobbo.GetFluidEmitterId world) world
                World.emitFluidParticles (SArray.init absorbed.Count (fun i -> absorbed[i])) (emitter.GetFluidEmitterId world) world
            | None -> ()
        ()