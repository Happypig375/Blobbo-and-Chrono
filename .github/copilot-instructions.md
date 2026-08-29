# Repository-wide instructions for GitHub Copilot

Read `AGENTS.md` at the repository root before making changes. It is the tool-neutral, authoritative
source for repository workflow, verification, and AI-assisted production policy.

## Coding Standard
Read `Standard.md` at the repository root for the complete coding standard.
Always create F# code files instead of other languages unless you are extending an existing C# project.

## Project Info
- Engine source: `Nu/Nu/` (F# project `Nu.fsproj`) with dependencies on `Nu/Nu.Math/` (C# project `Nu.Math.csproj`) and `Nu/Nu.Spine/` (C# project `Nu.Spine.csproj`)
- Engine tests: `Nu/Nu.Tests/` (F# project `Nu.Tests.fsproj`)
- World editor (can load any game project with code reload, builds them all when itself is built): `Nu/Nu.Gaia` (run via `dotnet run --project Nu/Nu.Gaia`)
- Game projects: `Projects/` directory (each a standalone `.fsproj`)
- Asset processor for Gaia and Game projects: `Nu/Nu.Pipe/` (F# project `Nu.Pipe.fsproj`)

## Build Commands
```bash
# Build Blobbo and Chrono
dotnet build "Projects/Blobbo and Chrono/Blobbo and Chrono.fsproj"

# Build game projects and world editor
dotnet build Nu/Nu.Gaia/Nu.Gaia.fsproj -f net10.0

# Build solution
dotnet build Nu.slnx
```

## Architecture
[DeepWiki: Nu Game Engine](https://deepwiki.com/bryanedds/Nu) is the authoritative reference for architecture and concepts.

**For any architecture question, dispatcher patterns, facet patterns, entity patterns, or engine internals — fetch the latest from DeepWiki before answering, rather than searching the codebase without context.** The DeepWiki content is more complete and up-to-date.

- Physics: Box2D.NET (2D) and Jolt Physics (3D). Aether Physics is considered legacy and not recommended for new projects.
  - Box2D API reference: https://box2d.org/documentation/
- ImSim property operators
  `.=` Set once (static), reapply on code reload
  `|=` Set once (initialize-once), NO reapply on code reload
  `@=` Set every frame (dynamic binding)
- Custom skills in `.github/skills/` document engine-specific patterns. Always update this file as well as any skill document if they are out of date, missing important patterns, need clarification, can be simplified, etc.
