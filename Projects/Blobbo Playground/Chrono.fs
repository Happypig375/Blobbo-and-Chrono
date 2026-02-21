namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

[<AutoOpen>]
module ChronoExtensions =
    type Entity with
        member this.GetHourglass world : Entity Address = this.Get (nameof this.Hourglass) world
        member this.SetHourglass (value : Entity Address) world = this.Set (nameof this.Hourglass) value world
        member this.Hourglass = lens (nameof this.Hourglass) this this.GetHourglass this.SetHourglass
type ChronoDispatcher () =
    inherit FluidEmitter2dDispatcher ()

    static member Properties =
        [define Entity.Size (v3 320f 320f 0f)
         define Entity.FluidParticleRadius 2f
         define Entity.Gravity (GravityOverride (v3 0f -0.5f 0f))
         define Entity.StaticImage Assets.Gameplay.Sand
         define Entity.Viscocity 1f
         define Entity.LinearDamping 0.5f
         define Entity.Hourglass Address.empty
         ]

    override _.Register (chrono, world) =
        let hourglass = World.createEntity<BoxBody2dDispatcher> (Some chrono.EntityAddress) DefaultOverlay (Some (Array.add "Hourglass" chrono.Surnames)) chrono.Group world
        chrono.SetHourglass hourglass.EntityAddress world
        hourglass.SetStaticImage Assets.Gameplay.Hourglass world
        hourglass.SetScale (v3 0.5f 0.5f 1f) world

    override _.Unregister (chrono, world) =
        tryResolve (chrono.GetHourglass world) chrono |> Option.iter (fun e -> World.destroyEntity e world)

    override _.RegisterPhysics (blobbo, world) =
        base.RegisterPhysics (blobbo, world)
        let position = blobbo.GetPosition world
        World.emitFluidParticles
            (SArray.init 400 (fun i ->
                let (x, y) = Math.DivRem (i, 20)
                { FluidParticlePosition = position + 4f * v3 (single x) (single y) 0f
                  FluidParticleVelocity = v3Zero
                  FluidParticleConfig = "Water" }))
            (blobbo.GetFluidEmitterId world) world

    override _.Update (blobbo, world) =
        let position = blobbo.GetPosition world
        let mutable newPosition = v3Zero
        let mutable particleCount = 0
        World.chooseFluidParticles (fun p ->
            newPosition <- newPosition + p.FluidParticlePosition
            particleCount <- inc particleCount
            ValueSome {
                p with
                    FluidParticleVelocity =
                        p.FluidParticleVelocity +
                        (0.1f * v3 (position - p.FluidParticlePosition).Magnitude 0f 0f).Transform
                            (Quaternion.CreateLookAt2d (position - p.FluidParticlePosition).V2) +
                        (if World.isKeyboardKeyDown KeyboardKey.J world then v3 -0.01f 0f 0f else v3Zero) +
                        (if World.isKeyboardKeyDown KeyboardKey.L world then v3 0.01f 0f 0f else v3Zero)
                })
            (blobbo.GetFluidEmitterId world) world
        blobbo.SetPosition (newPosition / single particleCount) world