# Blobbo and Chrono architecture

> **Status:** This document describes the currently implemented browser/audio integration shell. It does not define the product roadmap. Read `PLAN.md` first for the current game design, milestone order, target feature-timeline boundary, and compliance gates. Update this document as milestones replace prototype assumptions.

Nu remains the host for the window, render loop, and lifecycle. `CompositionRoot` is a
runnable integration shell started and stopped by `Program`; gameplay installs a shared
instance, pumps browser audio without blocking, and renders latest snapshot status.

* **Browser.** Desktop builds use an external browser process/extension and a transparent
  top-level overlay; mobile builds may provide an owned browser and composited surfaces.
  Both are represented by the narrow `IBrowserBridge` contract and `BrowserEvent` values.
  Platform window handles and IPC stay outside this project.
* **Audio.** `AudioIngress.Submit` accepts mono 48 kHz analysis samples, copies them into
  bounded, preallocated analysis and playback rings, and advances an authoritative `int64`
  source-sample clock. Each slot carries its absolute start position and valid count;
  producers drop new blocks under backpressure and consumers detect discontinuities. Playback timing
  uses the same mono clock; a future stereo adapter may interleave playback samples.
  `CompositionRoot.TryReadPlayback` exposes an allocation-free, non-blocking
  count/absolute-position drain for the private delayed-playback ring.
* **MuScriptor.** `MuScriptorCoordinator` consumes independent five-second windows with
  300 ms overlap and emits sample-positioned symbolic events. Defaults are
  `prelude_forcing=false`, beam 1, batch 1. `ISymbolicInference` is a placeholder seam
  for a future model and has no runtime dependency today.
* **Simulation.** `SimulationWorker` translates symbolic events into immutable render
  snapshots. Rendering reads the latest snapshot without taking a lock.

`NullBrowserBridge` and `NullInference` make the shell build and run offline. Failure and
overload are isolated to the corresponding bounded queue; platform browser/audio/model
adapters, click-through focus policy, and native compositing remain to be implemented per
platform. The desktop shell already requests SDL transparent and always-on-top window flags.
Start/stop operations are idempotent and serialized; worker shutdown joins fully. A future
inference adapter that cannot be interrupted must be treated as a non-restartable fault.
