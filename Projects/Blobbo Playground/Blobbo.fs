namespace BlobboPlayground
open System
open System.Numerics
open Prime
open Nu

type BlobboDispatcher () =
    inherit FluidEmitter2dDispatcher ()

    static member Properties =
        [define Entity.Size (v3 320f 320f 0f)
         define Entity.FluidParticleRadius 10f]

    override _.RegisterPhysics (blobbo, world) =
        base.RegisterPhysics (blobbo, world)
        let position = blobbo.GetPosition world
        World.emitFluidParticles
            (SArray.init 20 (fun i ->
                let (x, y) = Math.DivRem (i, 5)
                { FluidParticlePosition = position + 8f * v3 (single x) (single y) 0f
                  FluidParticleVelocity = v3Zero
                  GravityOverride = ValueNone }))
            (blobbo.GetFluidEmitterId world) world

    override _.Update (blobbo, world) =
        let position = blobbo.GetPosition world
        let mutable newPosition = v3Zero
        let mutable particleCount = 0
        World.mapFluidParticles (fun p ->
            newPosition <- newPosition + p.FluidParticlePosition
            particleCount <- inc particleCount
            { p with

                FluidParticleVelocity =
                    p.FluidParticleVelocity +
                    (v3 (position - p.FluidParticlePosition).Magnitude 0f 0f).Transform
                        (Quaternion.CreateLookAt2d (position - p.FluidParticlePosition).V2) +
                    (if World.isKeyboardKeyDown KeyboardKey.W world then v3 -5f 0f 0f else v3Zero) +
                    (if World.isKeyboardKeyDown KeyboardKey.D world then v3 5f 0f 0f else v3Zero)
                })
            (blobbo.GetFluidEmitterId world) world
