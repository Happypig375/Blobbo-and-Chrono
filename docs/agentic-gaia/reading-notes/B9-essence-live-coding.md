# B9. The Essence of Live Coding: Change the Program, Keep the State!: reading note (full text)

Short answer: a language-design paper, not an empirical study. It shows, in Haskell, how to swap a running program's code and carry its state across automatically, by comparing the old and new state types. The guarantee is only that the new state has the right type, not that it is sensible. It is about a toy-scale Haskell framework, not about game engines. Its value for us is the vocabulary and the limits of "keep the state" when code changes.

## Citation
Bärenz, M. (2020). The Essence of Live Coding: Change the Program, Keep the State! In *Proceedings of the 7th ACM SIGPLAN International Workshop on Reactive and Event-Based Languages and Systems (REBLS '20)*, 16 November 2020, virtual. ACM, 13 pages. https://doi.org/10.1145/3427763.3428312 . Author affiliation on the PDF: sonnen eServices GmbH, Germany. The paper is literate Haskell and ships its library: https://github.com/turion/essence-of-live-coding (not visited). An appendix is online (p. 10, footnote 7; not read).

## Versions
- One version read: the ACM Digital Library publisher PDF, 13 pages, downloaded by `litfetch` (`docs/agentic-gaia/litfetch-optional.json`, status `entitled`).
- Check date: 2026-11-02.
- **Page numbers.** The PDF file has 13 pages, but the footers print 2 to 14 (the first page carries the number 2). Below I use PDF file pages (1-13). Add one to get the printed number.

## Access and read depth
Full text via `litfetch`, campus access. I read all 13 pages of the main paper, including the code listings and the conclusion. The Haskell code was read for its idea, not run. The online appendix (Monad instance for the exception cell) and the GitHub library were not read. The mathematical detail of Sec. 4-5 (arrowized FRP, the exception monad, `foreverE`) was read at a high level only. I did not verify the type-theory claims.

## What the paper is
A functional-programming design and implementation paper (workshop paper at REBLS '20). It develops a small framework step by step, with short demos (a counter, a web server that counts visitors, a sine-wave generator printed as text). There is no study of programmers, no benchmark, no performance measurement, and no game engine. Evidence is the worked examples in the paper.

## Key results (with locators; PDF pages)
1. **Idea** (Intro, p. 1): the live-coding mantra is "Change the program. Keep the state." The author applies this motto strictly to build a general-purpose, type-safe framework with automatic state migration. The idea itself is not new: Erlang hot code swap and Elm do the same (p. 2).
2. **The problem** (Sec. 2, p. 2): in a dynamic language you can simply swap the step function and continue on the old state. But the new function may crash or misbehave on the old state. Tests are hard because state changes all the time. A static type checker would help, but the types of old and new state may differ.
3. **Schema migration idea** (p. 3): think of the program state as a database table with one row; code changes are schema migrations. Some data loss is unavoidable (a deleted column), and a new column needs a default.
4. **No universal migration without extra information** (p. 3): a function that takes only the old state and the new initial state, with no knowledge of types, can only throw the old state away (a "free theorem": it must be `const`, i.e. restart).
5. **Automatic, type-driven migration** (pp. 3-4): using Haskell generic programming (the `Data` and `Typeable` classes), the migration function matches constructor names and record field names. Same names are copied across; new fields take the value from the new program's initial state; nested structures are handled recursively; identical types are copied unchanged. It is under 50 lines. The user can add special cases (for example `Int` to `Integer`). Restrictions: state must be plain data (no functions); all functions must live in the step function, all data in the state (p. 4).
6. **Runtime** (Sec. 3, pp. 4-6): the live program sits in a concurrent variable (an `MVar`). A background thread steps it. To update, take the program out, migrate, put it back. `ghcid` or GHCi reload triggers this on file save (p. 5). A demo web server keeps its visitor count across a code change and starts a new field at `Nothing` (pp. 5-6).
7. **Composable state** (Sec. 4, pp. 6-8): programs are built from "cells" (effectful state machines). The state of a composite cell is built automatically from its parts, so users do not write a state type. Demo: change a sine wave's period mid-run and the phase does not jump (p. 8).
8. **Control flow in the state** (Sec. 5, pp. 8-10): "which phase am I in" is also stored in the state (using exceptions as switches), so reloading continues from the same phase instead of restarting. This is contrasted with "simplistic approaches" that forget control state on reload (p. 10).
9. **Tools** (Sec. 6, pp. 11-12): a debugger that can read and modify the whole live state; QuickCheck property tests of cells; and *migration tests*: run the new version's tests on the migrated state before switching the live system, using two threads (test and production) (p. 12).
10. **Stated limits** (Sec. 7, p. 12): the automatic migration "only guarantees that the new state will typecheck". Other properties of the old state are not guaranteed for the new one. Real use needs wrappers for web, OpenGL, gloss or audio loops. Refinement types are mentioned as future help.

## Verbatim quotes (checked against the PDF text; page numbers are PDF file pages)
1. (p. 2, Sec. 2) "Change the program. Keep the state (as far as possible)."
2. (p. 2, Sec. 2) "the new transition function may crash or otherwise misbehave on the old state"
3. (p. 12, Sec. 7) "The automatic migration only guarantees that the new state will typecheck."
4. (p. 12, Sec. 7) "To use the framework in any setting beyond a toy application, wrappers have to be written that explicitly integrate it in the external loops of existing frameworks"

## Limitations and how strong the evidence is
- Not an empirical study. No participants, no performance numbers, no comparison with other live-coding systems.
- Toy-scale demos. The author says so (Sec. 7, p. 12).
- Haskell-specific. The migration relies on Haskell's generic programming and a state made of plain data. It does not carry over directly to a language or engine where state includes functions, closures, pointers, handles or GPU resources.
- Migration is by names and types. Renaming a field loses its data. Two fields of the same type without names are ambiguous (p. 3). The meaning of a value (units, ranges) is not checked.
- The reload loop is cheap in the paper because the programs are tiny; the cost of recompiling a large project is not discussed.

## Relevance to the Gaia agent design
- **Core idea for hot reload.** When code changes, what happens to the live world? Gaia's `gaia_reload_code` recompiles the project's sources into the running world (about 25 s for Breakout per `design.md`). B9 names the design question clearly: which state survives, which resets, and who decides. For an agent this means: after a reload, check what was kept. Do not assume existing entities picked up the new code.
- **Type-driven migration lesson.** If a record or type changes shape (a new field in an F# model or entity property), the old values need a mapping. This paper's rule is: keep fields with the same name and type, take defaults for new ones, and lose removed ones. For Gaia this is an analogy to guide what an agent should check after changing a type, not something Gaia does. Whether F# Interactive or Nu's reload keeps old instances of a changed type is not stated here and needs testing in the editor.
- **Control state is state too.** The "which phase am I in" point matters for game logic that uses a state machine or screen transitions. After a reload the agent should check that the game is in the phase it expects (use `gaia_inspect` or `gaia_assert`), not only that numbers are right.
- **Test before you switch.** The migration-test idea (run the checks on the migrated state before making it live) matches Gaia's pattern of snapshot, eval, assert, then keep or `gaia_undo`. It supports taking a snapshot before every reload and asserting afterwards.
- **Type check is not correctness.** The paper's own limit (typecheck only) is a warning that a successful reload or a compiling `gaia_eval` does not mean the game is right. This agrees with the evidence elsewhere in the review that runtime checks beat "it compiles".
- Not covered: latency, runtime repair, alternatives/branching, MCP safety, games (only one footnote says FRP can be used for games).

## Corrections to the current literature-review.md entry (B9)
Entry: "Bärenz, M. (2020). *SPLASH Onward!*, 2-14. ... Depth: Scite metadata only (closed). Title thesis: 'change the program, keep the state'. Not read. Informs: state-preserving hot reload semantics."
- **Venue is wrong.** The PDF names the Reactive and Event-Based Languages and Systems workshop (REBLS '20), not Onward!. The DOI prefix 10.1145/3427763 is the REBLS proceedings. The review should say "REBLS '20 (ACM SIGPLAN workshop)". The "2-14" in the review matches the printed footer numbers (2 to 14).
- **"Not read" is now out of date.** It has been read in full.
- **Add:** type of paper (Haskell language-design paper, no empirical study); main finding (automatic type-driven state migration; guarantee limited to type correctness); scope (toy examples, not game engines).
- **"Informs: state-preserving hot reload semantics" is fair** as a general framing, but only as analogy: the paper does not touch game engines or F#.
