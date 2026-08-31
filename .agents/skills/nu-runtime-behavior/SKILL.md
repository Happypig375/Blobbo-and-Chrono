---
name: nu-runtime-behavior
description: >-
  Diagnose, implement, and test Nu runtime behavior involving physics, ImSim lifecycle, World
  construction, native integration, or entity-level behavior. Use when the subject is Nu rather than
  a backend in isolation.
---

# Nu runtime behavior

Read the repository-root `AGENTS.md`, [`Standard.md`](../../../Standard.md), and
`../nu-maintainer-workflow/SKILL.md` for engine or upstream work.

Restore the intended behavior through Nu's owning abstraction, then verify it at the level where users and
sample projects experience it. Exact physics packages, native loaders, and signatures are volatile: inspect
the checked-out `.fsproj`, backend modules, World integration, and tests before changing them. At the
upstream revision recorded in `.agents/context/nu-maintainer-evidence.md`, Nu used Box2D.NET for 2D and Jolt
for 3D, but that observation is not a timeless contract.

## Establish the behavioral model

1. Identify the supported public path: plugin, World, dispatcher, facet, entity, property, event, or service.
2. Find the previous implementation or nearest working analogue.
3. Write down the observable invariants: reachability, stability, collision/event delivery, geometry,
   ordering, resource lifetime, or reload behavior.
4. Separate units and scale conversions from tuning values.
5. Fix the cause before adding compensating damping, repeated impulses, corrective transforms, sleeps, or
   persistent flags.

Preserve intentional shape and joint representations unless evidence requires a change. Put shared physical
or protocol values in the closest existing constants module; do not duplicate package defaults as magic
numbers.

## World and lifecycle

- Use the normal curried World construction path and its real dependencies for integration tests. Do not add
  a renderer-specific, physics-specific, or test-only public constructor merely for convenience.
- Use existing ImSim declaration operators and initialization signals. Do not mirror lifecycle state in a
  new flag.
- Preserve API currying and established parameter order.
- Keep declaration names and entity addresses stable unless the behavior explicitly requires a migration.
- When an optional publication or rendering path is disabled, continue updating required simulation state
  while avoiding the optional allocation or payload. Test both branches.

## Choose the correct test level

Prefer the highest level that still isolates the claim:

- **Pure test:** conversions, deterministic domain functions, validation, serialization.
- **Backend test:** a capability intentionally exposed only by one backend, or a backend regression before
  Nu integration exists.
- **Nu integration test:** plugin + World + dispatcher/facet/entity behavior.
- **Sample/game validation:** controls, rendering, timing, editor reload, and preserved player-visible
  behavior.

A hand-built backend world does not prove Nu integration. Conversely, do not force a World test around a
backend-only capability. When testing a backend directly, leave a concise nearby note explaining why the
supported World path cannot exercise that capability.

Stub unrelated rendering or I/O only when the real World lifecycle and public construction path remain.
Assert meaningful outcomes over time rather than only construction. Use deterministic fixtures, bounded
steps, tolerances justified by units, and focused stress scenes.

## Native runtime and platform failures

When a native library must be resolved beside managed output:

- anchor the path to `AppContext.BaseDirectory`, not a process working directory;
- pass the concrete loader path to the owning runtime hint;
- distinguish a loader from an implementation/ICD and let existing manifests select the implementation;
- capture the first native error before cleanup can overwrite it;
- distinguish initialization, device creation, resource use, and teardown failures.

Do not encode a platform workaround as a universal rule without current source and platform evidence. Keep
the workaround local, document why the apparently simpler path fails, and test on the affected platform.

## Regression and manual coverage

Run the narrow test first, then the owning project and every sample that reaches the changed integration.
Package or backend migrations require manual preservation checks across affected sample games, not only a
successful engine build.

Report outcomes precisely. A suite that reports passed assertions and later crashes during teardown is not a
clean pass: record both facts, preserve the native error, and file or link an issue when it may represent a
defect. For interactive checks, record scene, controls, duration or steps, representative observations, and
runtime warnings/errors.

Before concluding, inspect the diff for unintended tuning, duplicate constants, compatibility aliases,
default-property repetition, project-reference expansion, source-order changes, and unrelated formatting.