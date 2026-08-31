---
name: nu-runtime-behavior
description: >-
  Diagnose, implement, and test Nu runtime behavior involving physics, ImSim lifecycle, World
  construction, or entity integration. Use when porting simulations, changing runtime initialization,
  or writing tests whose subject is Nu rather than a backend in isolation.
---

# Nu runtime behavior

Restore the intended runtime behavior through Nu's existing abstractions, then verify it at the same
level at which users experience it.

Nu currently uses Box2D.NET for 2D physics and Jolt Physics for 3D physics. Treat Aether Physics as a
legacy behavior reference, not as the backend for new work. Consult the
[Box2D API reference](https://box2d.org/documentation/) for backend semantics.

## Preserve the model

- Establish the reference behavior from the original implementation, documentation, and nearby Nu
  code before tuning values.
- Fix the cause before adding compensating damping, corrective transforms, repeated impulses, or
  persistent state. Add such behavior only when it is part of the intended model.
- When adapting a physical example to a different world scale, preserve its mechanics and geometry
  and scale dimensionful quantities consistently. Keep intentional Nu shape and joint representations
  unless evidence requires changing them.
- Reuse engine constants and put demo-specific shared values in the closest existing `Constants`
  module.

## Use Nu lifecycle and construction APIs

- When an integration test needs non-stub runtime behavior, call the existing curried `World.make`
  with its normal parameters, including an appropriate `SdlDeps` value. Do not add a renderer- or
  physics-specific World constructor solely to make a test convenient.
- Use the appropriate ImSim property operator and lifecycle signal. For init-only effects, prefer
  `world.DeclaredInitializing` over a new persistent flag that duplicates initialization state.
- Preserve established API currying and inline one-use forwarding aliases.

## Native runtime dependencies

When native libraries are copied or symlinked beside managed output, anchor their paths to
`AppContext.BaseDirectory`, rather than relying on bare names or the process working directory. Pass
the resulting concrete path to runtime hints such as `SDL_HINT_VULKAN_LIBRARY`. Capture native errors
before cleanup can clear them, and retain those errors in test failures. Preserve existing backend and
configuration switches when applying a path fix (for example, do not override `MoltenVk=false`).
Distinguish an `SDL_Init` video-device failure from a later Vulkan-library loading failure before
choosing the remedy.

## Test the Nu integration

- If the behavior under test belongs to a Nu game or engine integration, exercise the actual plugin,
  World, dispatchers, facets, and entities. A hand-built equivalent backend world tests the backend,
  not the Nu integration.
- Stub external dependencies such as rendering when necessary, but retain the real World lifecycle
  and public construction path.
- Assert meaningful outcomes such as reachability, stability over time, and spatial relationships.
  Isolate unrelated entities in focused stress tests without reconstructing the system being tested.