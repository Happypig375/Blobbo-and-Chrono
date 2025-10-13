namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

type BlobboMovement = Left | Still | Right
module [<AutoOpen>] BlobboExtensions =
    type Entity with
        member this.GetWorldFluidEmitter world : Entity Address = this.Get (nameof this.WorldFluidEmitter) world
        member this.SetWorldFluidEmitter (value : Entity Address) world = this.Set (nameof this.WorldFluidEmitter) value world
        member this.WorldFluidEmitter = lens (nameof this.WorldFluidEmitter) this this.GetWorldFluidEmitter this.SetWorldFluidEmitter
        member this.GetAbsorbing world : bool = this.Get (nameof this.Absorbing) world
        member this.SetAbsorbing (value : bool) world = this.Set (nameof this.Absorbing) value world
        member this.Absorbing = lens (nameof this.Absorbing) this this.GetAbsorbing this.SetAbsorbing
        member this.GetMovement world : BlobboMovement = this.Get (nameof this.Movement) world
        member this.SetMovement (value : BlobboMovement) world = this.Set (nameof this.Movement) value world
        member this.Movement = lens (nameof this.Movement) this this.GetMovement this.SetMovement

type BlobboDispatcher () =
    inherit FluidEmitter2dDispatcher ()

    static member Properties =
        [define Entity.Size (v3 60f 60f 0f)
         define Entity.FluidParticleRadius 5f
         define Entity.GravityOverride (Some (v3 0f -1f 0f))
         define Entity.StaticImage Assets.Default.Fluid
         define Entity.Color (colorDup 0.8f)
         define Entity.Viscocity 2f
         define Entity.LinearDamping 0.75f
         define Entity.WorldFluidEmitter Address.empty
         define Entity.Absorbing false
         define Entity.Movement Still
         ]

    override _.Register (blobbo, world) =
        base.Register (blobbo, world)

        // eject out of bounds particles to the world fluid emitter
        World.monitor (fun event world ->
            let blobbo : Entity = event.Subscriber
            match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
            | Some (emitter : Entity) ->
                World.emitFluidParticles event.Data.OutOfBoundsParticles (emitter.GetFluidEmitterId world) world
            | None -> ()
            Cascade) blobbo.FluidEmitterUpdateEvent blobbo world

        // initialize with 400 particles
        let position = blobbo.GetPosition world
        let sideLength = 20
        blobbo.SetFluidParticles
            (SArray.init (sideLength * sideLength) (fun i ->
                let (x, y) = Math.DivRem (i, sideLength)
                { FluidParticlePosition = position + 2f * v3 (x - sideLength / 2 |> single) (y - sideLength / 2 |> single) 0f
                  FluidParticleVelocity = v3Zero
                  GravityOverride = ValueNone })) world

    override _.Update (blobbo, world) =
        let position = blobbo.GetPosition world
        let mutable newPosition = v3Zero
        let mutable particleCount = 0

        // gravitate particles towards center for blob shape
        let movement =
            match blobbo.GetMovement world with
            | Left -> v3 -0.01f 0f 0f
            | Still -> v3Zero
            | Right -> v3 0.01f 0f 0f
        World.chooseFluidParticles (fun p ->
            newPosition <- newPosition + p.FluidParticlePosition
            particleCount <- inc particleCount
            ValueSome {
                p with
                    FluidParticleVelocity =
                        p.FluidParticleVelocity +
                        (0.001f * v3 (position - p.FluidParticlePosition).Magnitude 0f 0f).Transform
                            (Quaternion.CreateLookAt2d (position - p.FluidParticlePosition).V2) + movement
                })
            (blobbo.GetFluidEmitterId world) world

        // update center to be average of particle positions
        blobbo.SetPosition (newPosition / single particleCount) world

        // when absorbing, convert world fluid emitter particles to blobbo particles
        if blobbo.GetAbsorbing world then
            let bounds = blobbo.GetBounds world
            let absorbed = ResizeArray 32
            match tryResolve (blobbo.GetWorldFluidEmitter world) blobbo with
            | Some (emitter : Entity) ->
                World.chooseFluidParticles (fun p ->
                    if bounds.Contains p.FluidParticlePosition <> ContainmentType.Disjoint then
                        absorbed.Add p
                        ValueNone
                    else ValueSome p) (emitter.GetFluidEmitterId world) world
                World.emitFluidParticles (SArray.init absorbed.Count (fun i -> absorbed[i])) (blobbo.GetFluidEmitterId world) world
            | None -> ()