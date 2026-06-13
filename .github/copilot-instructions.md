# Repository-wide instructions for GitHub Copilot

## Coding Standard
Always follow `Standard.md` at the repository root. Key rules:
- 4-space indentation, 120-column line limit, Lisp-style bracing
- `UpperCamelCase` for types/modules, `lowerCamelCase` for functions/variables
- Prefer immutable types and referentially-transparent functions
- Avoid class/struct types and instance members unless specifically needed
- Suffix option bindings with `Opt`, prefix try-functions with `try`
- `open` statements at top of file below namespace declaration
- Handle intentional case first in match/if expressions

## Project Info
- Language: F# targeting .NET 10.0
- Build tool: `dotnet build`
- Solution: `Nu.sln` at repo root
- World editor (can load any game project with code reload, builds them all when itself is built): `Nu/Nu.Gaia` (run via `dotnet run --project Nu/Nu.Gaia`)
- Engine source: `Nu/Nu/` (F# project `Nu.fsproj`)
- Asset processor: `Nu/Nu.Pipe/` (F# project `Nu.Pipe.fsproj`)
- Game projects: `Projects/` directory (each a standalone `.fsproj`)

## Build Commands
```bash
# Build game projects and world editor
dotnet build Nu/Nu.Gaia/Nu.Gaia.fsproj -f net10.0

# Build solution
dotnet build Nu.sln
```

## Architecture
- Entity hierarchy: Game → Screen → Group → Entity — each with its own dispatcher type. There is only one game dispatcher. Screen and Entity dispatchers are most common, where only one screen is active at once; Group is mainly for loading entities together such as from a file so group dispatchers are rare.
  - **ImSim** programming model (immediate-mode simulation) — entities declared inline in `Process` methods with `World.begin*`/`World.do*`/`World.end*`
    - Game dispatcher: `GameDispatcherImSim` → `Process` method with `World.beginScreen/endScreen`.
    - Screen dispatchers: `ScreenDispatcherImSim` → `Process` method with `World.beginGroup/endGroup`
    - Entity dispatchers: `Entity2dDispatcherImSim` / `Entity3dDispatcherImSim`
  - **MMCC** programming model (Model-Message-Command-Content, Elm-like MVU pattern): `Entity2dDispatcher<'model, 'message, 'command>` / `Entity3dDispatcher<'model, 'message, 'command>` etc — see `Projects/Nelmish`, `Projects/Breakout Mmcc`, `Projects/Twenty 48`, `Projects/Blaze Vector Mmcc` in increasing order of complexity
  - **Classic** programming model (without the conveniences of ImSim or MMCC): non-generic `Entity2dDispatcher` / `Entity3dDispatcher` etc — entities declared in `Register` methods, with event handlers registered via `World.sense` and friends. The built-in entity dispatchers in `Nu/Nu/World/WorldDispatchers.fs` are written in this style.
- Facets: reusable entity behavior via `Facet` base class (Classic Nu model, not ImSim)
- Physics: Box2D.NET (2D) and Jolt Physics (3D). Aether Physics is considered legacy and not recommended for new projects.
- Custom skills in `.github/skills/` document engine-specific patterns. Always update this file as well as any skill document if they are out of date, missing important patterns, need clarification, can be simplified, etc.

## ImSim Property Operators
| Operator | Meaning |
|----------|---------|
| `.=` | Set once (static), reapply on code reload |
| `|=` | Set once (initialize-once), NO reapply on code reload |
| `@=` | Set every frame (dynamic binding) |
