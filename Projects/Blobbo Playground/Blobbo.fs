namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

type BlobboMovement = Left | Still | Right
type Absorption = Absorbing | Equilibrium | Emitting
module [<AutoOpen>] BlobboExtensions =
    type Entity with
        member this.GetWorldFluidEmitter world : Entity Address = this.Get (nameof this.WorldFluidEmitter) world
        member this.SetWorldFluidEmitter (value : Entity Address) world = this.Set (nameof this.WorldFluidEmitter) value world
        member this.WorldFluidEmitter = lens (nameof this.WorldFluidEmitter) this this.GetWorldFluidEmitter this.SetWorldFluidEmitter
        member this.GetAbsorption world : Absorption = this.Get (nameof this.Absorption) world
        member this.SetAbsorption (value : Absorption) world = this.Set (nameof this.Absorption) value world
        member this.Absorption = lens (nameof this.Absorption) this this.GetAbsorption this.SetAbsorption
        member this.GetMovement world : BlobboMovement = this.Get (nameof this.Movement) world
        member this.SetMovement (value : BlobboMovement) world = this.Set (nameof this.Movement) value world
        member this.Movement = lens (nameof this.Movement) this this.GetMovement this.SetMovement
        member this.GetChargeTarget world : Vector3 option = this.Get (nameof this.ChargeTarget) world
        member this.SetChargeTarget (value : Vector3 option) world = this.Set (nameof this.ChargeTarget) value world
        member this.ChargeTarget = lens (nameof this.ChargeTarget) this this.GetChargeTarget this.SetChargeTarget
        member this.ShootEvent = stoa<Vector3> "Shoot/Event" --> this
        member this.LeapEvent = stoa<Vector3> "Leap/Event" --> this

type BlobboDispatcher () =
    inherit FluidEmitter2dDispatcher ()

    static let minBlobboSize = 3

    static member Properties =
        [define Entity.Size (v3 60f 60f 0f)
         //define Entity.FluidParticleRadius 5f
         //define Entity.Gravity (GravityOverride (v3 0f -1f 0f))
         define Entity.StaticImage Assets.Default.Fluid
         define Entity.Color (colorDup 0.8f)
         //define Entity.LinearDamping 0.75f
         define Entity.WorldFluidEmitter Address.empty
         define Entity.Absorption Equilibrium
         define Entity.Movement Still
         define Entity.ChargeTarget None
         ]

    override _.Register (blobbo, world) =
        base.Register (blobbo, world)

        World.monitor (fun event world ->
            // eject out of bounds particles to the world fluid emitter
            let blobbo : Entity = event.Subscriber
            let emitter = tryResolve (blobbo.GetWorldFluidEmitter world) blobbo |> Option.get
            World.emitFluidParticles (event.Data.OutOfBoundsParticles |> SArray.map (fun p -> { p with FluidParticleConfig = "Water" })) (emitter.GetFluidEmitterId world) world
            
            // detect ground for allowing leaping
            let groundDirection = blobbo.GetGravity world |> Gravity.localize (World.getGravity2d world) |> _.Normalized
            let up = -groundDirection
            Cascade) blobbo.FluidEmitterUpdateEvent blobbo world

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

        // leap when on ground
        World.monitor (fun event world ->
            if World.getFluidEmitterFluidGrounded (blobbo.GetFluidEmitterId world) world then
                let blobbo : Entity = event.Subscriber
                let movement = (event.Data - blobbo.GetPosition world : Vector3) * 0.015f
                World.chooseFluidParticles (fun p ->
                    ValueSome { p with FluidParticleVelocity = p.FluidParticleVelocity + movement })
                    (blobbo.GetFluidEmitterId world) world
            Cascade) blobbo.LeapEvent blobbo world

        // initialize with 100 particles
        let position = blobbo.GetPosition world
        let sideLength = 10
        blobbo.SetFluidParticles
            (SArray.init (sideLength * sideLength) (fun i ->
                let (x, y) = Math.DivRem (i, sideLength)
                { FluidParticlePosition = position + 2f * v3 (x - sideLength / 2 |> single) (y - sideLength / 2 |> single) 0f
                  FluidParticleVelocity = v3Zero
                  FluidParticleConfig = "Oil" })) world

    override _.Update (blobbo, world) =
        let mutable newPosition = v3Zero
        let mutable particleCount = 0

        // gravitate particles towards center for blob shape
        let movement =
            match blobbo.GetMovement world with
            | Left -> v3 -1f 0f 0f
            | Still -> v3Zero
            | Right -> v3 1f 0f 0f
        World.chooseFluidParticles (fun p ->
            newPosition <- newPosition + p.FluidParticlePosition
            particleCount <- inc particleCount
            ValueSome { p with FluidParticleVelocity = p.FluidParticleVelocity + movement })
            (blobbo.GetFluidEmitterId world) world

        // update center to be average of particle positions
        if particleCount > 0 then blobbo.SetPosition (newPosition / single particleCount) world

        match blobbo.GetAbsorption world with
        | Absorbing ->
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
        | Equilibrium -> ()
        | Emitting ->
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

    override _.Render (pass, blobbo, world) =
        base.Render (pass, blobbo, world)

        // display charge arrow
        match blobbo.GetChargeTarget world with
        | Some p2 ->
            let p1 = blobbo.GetPosition world
            let arrowRatio = 70f / 177f
            let difference = p2 - p1
            let distance = difference.Magnitude
            let mutable transform = Transform.makeIntuitive false ((p1 + p2) / 2f) v3One v3Zero (v3 distance (distance * arrowRatio) 0f) (v3 0f 0f (atan2 difference.Y difference.X)) (blobbo.GetElevation world)
            let mutable insetClipOpt = ValueNone
            let mutable color = colorOne
            let mutable emission = colorZero
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, Assets.Gameplay.WaterArrow, &transform, &insetClipOpt, &insetClipOpt, Assets.Gameplay.WaterArrow, &color, Transparent, &emission, Unflipped, world)
        | None -> ()