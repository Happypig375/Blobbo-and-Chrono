// Nu Game Engine.
// Required Notice:
// Copyright (C) Bryan Edds.
// Nu Game Engine is licensed under the Nu Game Engine Noncommercial License.
// See https://github.com/bryanedds/Nu/blob/master/License.md.

namespace BlobboPlayground.Tests
open System.Numerics
open NUnit.Framework
open Prime
open Nu
open BlobboPlayground
module M1ExperimentTests =

    [<Test>]
    let ``M1 deterministic verification passes`` () =
        let result = M1Verification.evaluate ()
        Assert.That (result.Passed, Is.True, String.concat "\n" result.Checks)

    [<Test>]
    let ``Simplified candidates materially reduce constraint cost`` () =
        Assert.That (M1Topology.jointCount SimplifiedRing, Is.EqualTo 24)
        Assert.That (M1Topology.constraintReduction SimplifiedRing, Is.GreaterThan 0.95f)
        Assert.That (M1Topology.jointCount StableHull, Is.Zero)

    [<Test>]
    let ``Touch samples use the common pointer abstraction`` () =
        let sample = M1PointerInput.fromTouch 7 42L (v2 12.0f -8.0f) PointerHeld
        Assert.That (sample.Tick, Is.EqualTo 7)
        Assert.That (sample.Position, Is.EqualTo (Vector2 (12.0f, -8.0f)))
        Assert.That (sample.Phase, Is.EqualTo PointerHeld)
        Assert.That (sample.Device, Is.EqualTo (TouchPointer 42L))

    [<Test>]
    let ``Magnitude clamp enforces configured limits`` () =
        let clamped = M1Control.clampMagnitude 5.0f (v2 30.0f 40.0f)
        Assert.That (clamped.Length (), (Is.EqualTo 5.0f).Within 0.0001f)

    [<Test>]
    let ``Release impulse preserves candidate direction rules`` () =
        let configuration = M1ControlConfiguration.defaultConfiguration
        let pointerVelocity = v2 100.0f 0.0f
        let grab = M1Control.releaseImpulse configuration GrabThrow v2Zero v2Zero pointerVelocity
        let pull =
            M1Control.releaseImpulse
                configuration
                PullSling
                v2Zero
                (v2 -100.0f 0.0f)
                pointerVelocity
        let swipe = M1Control.releaseImpulse configuration SwipeSmack v2Zero v2Zero pointerVelocity
        Assert.That (grab.X, (Is.EqualTo 16.0f).Within 0.0001f)
        Assert.That (pull.X, (Is.EqualTo 280.0f).Within 0.0001f)
        Assert.That (swipe.X, (Is.EqualTo 20.0f).Within 0.0001f)
        Assert.That (grab.Y, Is.Zero)
        Assert.That (pull.Y, Is.Zero)
        Assert.That (swipe.Y, Is.Zero)

    [<Test>]
    let ``Paused control step does not consume a release`` () =
        let configuration = M1ControlConfiguration.defaultConfiguration
        let activeSample =
            { Tick = 0
              Position = v2 0.0f 0.0f
              Phase = PointerPressed
              Device = MousePointer }
        let active = M1Control.step configuration GrabThrow v2Zero v2Zero activeSample ControlInactive
        let pausedSample = { activeSample with Tick = 1; Phase = PointerReleased; Position = v2 40.0f 0.0f }
        let paused = M1Control.stepWhenAdvancing false configuration GrabThrow v2Zero v2Zero pausedSample active.State
        Assert.That (paused.State, Is.EqualTo active.State)
        Assert.That (paused.Released, Is.False)
        Assert.That (paused.Impulse, Is.EqualTo v2Zero)