namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

type BlobboDoubleSizeDispatcher () =
    inherit FluidEmitter2dDispatcher ()

    static member Properties =
        [define Entity.Size (v3 320f 320f 0f)
         define Entity.FluidParticleRadius 10f
         define Entity.Gravity (GravityOverride (v3 0f -1f 0f))
         define Entity.StaticImage Assets.Default.Ball
         define Entity.FluidParticleImageSizeOverride (Some (v2 2f 2f))
         define Entity.Viscocity 1f
         define Entity.LinearDamping 0.5f
         ]

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
                        (0.001f * v3 (position - p.FluidParticlePosition).Magnitude 0f 0f).Transform
                            (Quaternion.CreateLookAt2d (position - p.FluidParticlePosition).V2) +
                        (if World.isKeyboardKeyDown KeyboardKey.A world then v3 -0.01f 0f 0f else v3Zero) +
                        (if World.isKeyboardKeyDown KeyboardKey.D world then v3 0.01f 0f 0f else v3Zero)
                })
            (blobbo.GetFluidEmitterId world) world
        blobbo.SetPosition (newPosition / single particleCount) world