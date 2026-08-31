# Repository agent instructions

## Scope

These instructions apply repository-wide. A nested `AGENTS.md` or `AGENTS.override.md` takes
precedence in its directory.

Before changing code or assets:

- Read `Standard.md` and follow its F# correctness, consistency, simplicity, and testing rules.
- Read `.agents/skills/nu-quickstart/SKILL.md` before implementing unfamiliar Nu patterns.
- Read `.agents/skills/nu-runtime-behavior/SKILL.md` for physics ports, World construction,
  ImSim lifecycle behavior, or Nu runtime integration tests.
- For work under `Projects/Blobbo and Chrono/`, read
  `Projects/Blobbo and Chrono/ARCHITECTURE.md` first and update it when architecture changes.
- Inspect the relevant implementation, tests, constants, lifecycle facilities, and recent history
  before introducing a new abstraction.

Preserve unrelated worktree changes. If work must be committed on another branch while local changes
remain, use a separate worktree and stage only the requested files.

## Repository map

- Nu engine: `Nu/Nu/` (`Nu.fsproj`)
- Math support: `Nu/Nu.Math/` (`Nu.Math.csproj`, C#)
- Spine support: `Nu/Nu.Spine/` (`Nu.Spine.csproj`, C#)
- Nu engine tests: `Nu/Nu.Tests/`
- World editor: `Nu/Nu.Gaia/` (loads game projects with code reload and builds them when Gaia builds)
- Asset processor: `Nu/Nu.Pipe/`
- Game projects: `Projects/` (each is a standalone `.fsproj`)
- Blobbo and Chrono: `Projects/Blobbo and Chrono/`

Useful commands:

```bash
dotnet tool restore
dotnet build "Projects/Blobbo and Chrono/Blobbo and Chrono.fsproj"
dotnet build Nu/Nu.Gaia/Nu.Gaia.fsproj -f net10.0
dotnet run --project Nu/Nu.Gaia
dotnet test Nu/Nu.Tests/Nu.Tests.fsproj
dotnet build Nu.slnx
```

## Validation

Run the narrowest relevant build or test first, then broader verification when practical. Test
observable behavior and regressions, not only construction or implementation details. Never claim
validation that was not run; record environmental blockers precisely.

Interactive testing is required when implementing a change and before submitting an upstream PR.
Exercise relevant controls such as clicks or drags, record the interaction, and inspect representative
frames rather than only the end state. Test dispatcher, facet, and default-asset changes in Gaia.

## Nu references

- Use the [upstream Nu wiki](https://github.com/bryanedds/Nu/wiki) for maintained usage guidance.
  Consult [Assets and the Asset Graph](https://github.com/bryanedds/Nu/wiki/Assets-and-the-Asset-Graph)
  when changing asset discovery, packages, or `AssetGraph.nuag`.
- Use [DeepWiki: Nu Game Engine](https://deepwiki.com/bryanedds/Nu) as the architecture and concepts
  index. Fetch its latest content before answering questions about engine internals, architecture,
  dispatchers, facets, or entities. For rendering-asset failures, start with
  [Texture and Asset Loading](https://deepwiki.com/bryanedds/Nu/4.5-texture-and-asset-loading).
- Treat the checked-out source, compiler, and tests as final authority when published guidance and the
  current revision differ.

## Default asset propagation

`Nu/Nu.Gaia/Assets/Default/` is the canonical default-asset source. After merging upstream engine
changes that touch default assets or shaders, and before runtime validation of affected projects, run
the matching propagation script from the repository root:

```bash
PropagateDefaultAssets.Windows.bat
./PropagateDefaultAssets.Linux.sh
```

Review and commit the resulting project asset changes; they are not disposable build output. Rebuild
the relevant project after propagation, then launch it. A successful compile alone does not prove
that runtime shader and asset requirements are present.

## Repository conventions

- Create F# source files by default. Use another language only when extending an existing project in
  that language, such as `Nu.Math` or `Nu.Spine` in C#.
- Preserve established F# API shapes, including currying.
- Put shared constants in the closest existing `Constants` module and reuse engine constants where
  available. Inline a value used only once instead of adding a forwarding alias.
- Put reusable task-specific guidance in a focused `.agents/skills/<name>/SKILL.md`; keep this file
  limited to repository-wide rules and routing.
- Keep upstream PR diffs minimal. End only C-style files with a newline; leave F# and Markdown files
  without one.

## AI-assisted production policy

Use AI to improve the project's ability to author, inspect, revise, and verify systems. For durable,
high-salience, frequently revised, or broadly reused work, prefer editable structured source over
opaque terminal output: executable code, typed data, scene graphs, geometry, shaders, rigs, semantic
events, generators, and explicit parameters or constraints.

Directly generated media is suitable for exploration, references, mock-ups, and clearly marked
placeholders. When generated work becomes durable or player-facing:

- preserve the source, parameters, constraints, seeds, source assets, and local overrides needed to
  reproduce and intentionally revise it;
- use compilers, tests, solvers, physics engines, renderers, and deterministic pipelines for results
  with checkable semantics;
- keep provenance and licensing information sufficient for audit and remove or clearly track
  placeholders before release;
- comply with storefront and platform attribution, licensing, and disclosure requirements;
- keep authoritative narrative and world state structured and validated rather than allowing
  unconstrained generated prose to mutate it;
- do not add runtime remote inference without an explicit architectural decision.

A change involving AI-assisted systems or content is complete only when its important outputs are
editable and reproducible at the appropriate level, its invariants and failure modes are discoverable,
and it has executable validation proportionate to its risk.

When a recurring correction reveals durable guidance, update the closest applicable `AGENTS.md` or
skill instead of relying on future prompts to repeat it.