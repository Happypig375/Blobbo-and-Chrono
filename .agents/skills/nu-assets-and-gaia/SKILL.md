---
name: nu-assets-and-gaia
description: >-
  Change Nu asset graphs, default assets, shaders, serialized entity trees, Gaia loading, or code-reload
  behavior. Use when a compile can succeed while editor or runtime assets still fail.
---

# Nu assets and Gaia

Read the repository-root `AGENTS.md`, [`Standard.md`](../../../Standard.md), and the closest project
`AGENTS.md`. Treat project output, asset packages, `AssetGraph.nuag`, serialized entity trees, default assets,
Gaia loading, and code reload as one integration surface.

## Source and authority

- Use asset tags and the project's asset graph. Do not introduce runtime file-relative lookup because it
  happened to work from one working directory.
- `Nu/Nu.Gaia/Assets/Default/` is the canonical source for Nu default assets in this repository.
- A project's copied default assets are source propagated from that canonical directory, not disposable
  build output.
- Current source and a successful runtime load override stale wiki or generated documentation.
- Inspect both the owning project and Gaia's generated-project/template path when a change affects new games.

## `.nugroup` and typed code

Use `.nugroup` for authored Nu entity trees that benefit from Gaia editing and serialization: scene shells,
test rooms, rigs, archetypes, pattern templates, local geometry, dressing, and named anchors.

Keep these responsibilities in typed, versioned systems rather than making a serialized scene their sole
authority:

- generation keys and seed derivation;
- media/analyzer provenance;
- pattern contracts, safe-path policy, and budgets;
- validation and difficulty mapping;
- runtime mutation history and culling;
- queue/session state;
- complete save semantics.

Prefer the hybrid:

```text
authored .nugroup + typed contract + deterministic parameters
-> immutable blueprint
-> Nu instantiation and typed overrides
-> bounded mutable runtime state
```

Do not hand-edit a large serialized tree when Gaia is the safer authoring path. When text editing is
necessary, preserve symbolic syntax, dispatcher/property names, addresses, ordering, and round-trip
loadability.

## Default asset propagation

After an upstream change to default assets, shaders, or their declarations, run the matching script from the
repository root:

```bash
PropagateDefaultAssets.Windows.bat
./PropagateDefaultAssets.Linux.sh
```

Then inspect the propagated diff. Commit intended project copies; do not discard them as generated output.
Do not propagate unrelated local changes from the canonical default directory.

## Validation sequence

1. Build the owning game or sample.
2. Build Gaia when its loader, template, defaults, or plugin integration is affected.
3. Load the project in Gaia and open the affected screen/group/entity.
4. Exercise code reload and confirm declared properties retain the intended `.=` / `|=` / `@=` behavior.
5. Launch the normal runtime path from a representative working directory.
6. Inspect logs and representative frames for missing packages, tags, shaders, native libraries, dispatcher
   resolution, or serialization failures.
7. Round-trip or reload serialized entity trees when their schema or defaults changed.

A compile-only result is insufficient for assets and shaders. A runtime-only result is also insufficient when
Gaia authoring or reload is part of the supported workflow.

## Failure diagnosis

Trace failures in this order:

- asset tag and package declaration;
- asset graph entry and build action;
- source/default asset presence;
- copied output beside the executable;
- dispatcher/facet/plugin availability;
- serialized address and property names;
- shader/native dependency availability;
- working-directory assumptions.

Fix the owning layer rather than adding fallback file paths or duplicate assets. Record the exact missing tag,
package, output path, or loader error before cleanup or fallback behavior obscures it.