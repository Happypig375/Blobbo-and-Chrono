# Repository agent instructions

## Scope

These instructions apply repository-wide. More specific instructions in a nested `AGENTS.md` or
`AGENTS.override.md` take precedence for work in that directory.

Before changing code or assets:

- Read `Standard.md` and follow its F# correctness, consistency, simplicity, and testing rules.
- Read `.github/skills/nu-quickstart/SKILL.md` before implementing unfamiliar Nu patterns.
- For work under `Projects/Blobbo and Chrono/`, read
  `Projects/Blobbo and Chrono/ARCHITECTURE.md` first and keep it current when architecture changes.
- Prefer the narrowest relevant build or test first, followed by broader verification when practical.

Create F# source files by default. Use another language only when extending an existing project in
that language, such as `Nu.Math` or `Nu.Spine` in C#.

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

Do not claim validation that was not run. Record environmental blockers precisely.

## Nu references and engine conventions

- Use the [upstream Nu wiki](https://github.com/bryanedds/Nu/wiki) for maintained usage guidance.
  In particular, consult [Assets and the Asset Graph](https://github.com/bryanedds/Nu/wiki/Assets-and-the-Asset-Graph)
  when changing asset discovery, packages, or `AssetGraph.nuag`.
- Use [DeepWiki: Nu Game Engine](https://deepwiki.com/bryanedds/Nu) as the architecture and concepts
  index. Fetch its latest content before answering questions about engine internals, architecture,
  dispatchers, facets, or entities. For rendering-asset failures, start with
  [Texture and Asset Loading](https://deepwiki.com/bryanedds/Nu/4.5-texture-and-asset-loading).
- Treat the checked-out source, compiler, and tests as the final authority when published guidance and
  the current revision differ.
- Nu uses Box2D.NET for 2D physics and Jolt Physics for 3D physics. Treat Aether Physics as legacy and
  avoid it for new work. Consult the [Box2D API reference](https://box2d.org/documentation/) when needed.
- ImSim property operators have distinct lifetimes: `.=` sets once and reapplies after code reload;
  `|=` initializes once without reapplying after code reload; `@=` binds dynamically every frame.
- Keep relevant documents in `.github/skills/` current when an engine pattern changes or a recurring
  correction reveals missing, confusing, or obsolete guidance.

## Default asset propagation

`Nu/Nu.Gaia/Assets/Default/` is the canonical default-asset source. After merging upstream engine
changes that touch default assets or shaders, and before runtime validation of projects affected by
such a merge, run the matching propagation script from the repository root:

```bash
PropagateDefaultAssets.Windows.bat
./PropagateDefaultAssets.Linux.sh
```

Review and commit the resulting project asset changes; they are not disposable build output. Rebuild
the relevant project after propagation, then launch it. A successful compile alone does not prove that
runtime shader and asset requirements are present.

## AI-assisted production policy

### Objective

Use AI primarily to increase the project's ability to author, inspect, revise, and verify systems.
Do not default to replacing durable creative source with opaque terminal output.

The preferred pipeline is:

```text
human intent and taste
    -> AI-assisted analysis, design, and implementation
    -> editable generators, semantic primitives, parameters, and constraints
    -> deterministic or inspectable execution
    -> player-facing output
```

This is a production default, not a categorical ban on direct generative media.

### Choose representation according to expected value

Before generating content, consider:

- **Longevity:** how long the result will remain useful.
- **Revision probability:** how often it is likely to change.
- **Salience:** how strongly players will judge the project by it.
- **Systemic reuse:** how many outputs can benefit from one correction.

For low-longevity, low-salience, disposable work, direct image, video, audio, or prose generation may
be appropriate for exploration, references, storyboards, mock-ups, and clearly marked placeholders.

As any of the four factors rises, prefer a structured representation that preserves the causes of the
output rather than only its final surface.

### Prefer generative source over terminal output

Prefer producing or modifying:

- F# or other executable source;
- typed data and explicit world state;
- scene graphs, geometry, shaders, material graphs, and procedural models;
- rigs, animation curves, cameras, trajectories, and simulations;
- semantic events, storylets, grammars, and surface-realization templates;
- musical notes, motifs, timing, and synthesis or scoring rules;
- reusable editor tooling, importers, validators, and debugging views.

Preserve the generator, parameters, constraints, seeds, source assets, and local overrides needed to
reproduce and intentionally change the result. A render, export, screenshot, recording, or generated
paragraph is not a substitute for its editable source when the source is practical and valuable.

A strong content architecture usually has:

```text
human-controlled primitives
    + human-controlled combinators
    + explicit constraints
    + composition-specific or hero overrides
    -> many coherent outputs
```

The primitives may begin as AI suggestions, and the combinator may be largely AI-implemented. The
maintainer must nevertheless be able to understand, test, and deliberately alter the relevant output
distribution. Generating many candidates and selecting one with minor cleanup is not equivalent to
controlling the system that produced them.

### Separate statistical proposal from rule-based execution

Use language and vision models for fuzzy tasks such as interpreting goals, proposing designs, finding
patterns, exploring alternatives, and criticizing results. Use the appropriate exact system for work
with checkable semantics:

- compiler and tests for code;
- arithmetic, computer algebra, constraint solvers, or proof assistants for mathematics;
- scene state, geometry, physics, and renderers for spatially consistent graphics and video;
- typed game state and validated transitions for simulation and narrative;
- deterministic asset pipelines for reproducible builds.

Prefer an iterative loop:

```text
propose -> execute -> inspect evidence -> revise
```

Do not ask a model to approximate a result that an available deterministic tool can calculate or
validate directly.

### Player-facing creative output

Direct model output is especially suitable for internal visualization and rapid rejection of weak
ideas. It should not silently become final high-salience game content.

For durable player-facing art, animation, audio, narrative, localization, UI, or marketing material:

- default to human-controlled structured source and an inspectable production pipeline;
- use model-generated media as reference or prototype unless the project explicitly chooses otherwise;
- remove or clearly track placeholders before release;
- keep provenance and licensing information sufficient for audit;
- comply with storefront, platform, attribution, and disclosure requirements instead of designing a
  pipeline merely to evade them;
- avoid adding a runtime remote-inference dependency without an explicit architectural decision.

For procedural narrative, prefer:

```text
authoritative structured state
    -> validated semantic event
    -> controlled surface realization
```

Do not allow unconstrained generated prose to become authoritative world state by accident.

### Review and completion criteria

A change involving AI-assisted systems or content is not complete until the relevant items are true:

- Important outputs are reproducible or their intentional nondeterminism is documented.
- The source representation is editable at the level where likely revisions will occur.
- Parameters, invariants, failure modes, and local overrides are discoverable.
- AI-assisted code has tests or another executable validation appropriate to its risk.
- Visual or systemic generators have stable seeds, fixtures, debug views, snapshots, or other practical
  inspection support when appropriate.
- Generated placeholders cannot be mistaken for approved final assets.
- Architecture and production documentation reflect durable decisions.
- The implementation follows `Standard.md` and the relevant Nu skill guidance.

When a recurring correction reveals a durable repository rule, update the closest applicable
`AGENTS.md` rather than relying on future prompts to repeat it.
