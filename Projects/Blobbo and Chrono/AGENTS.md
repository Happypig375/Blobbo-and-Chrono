# Blobbo and Chrono agent instructions

These instructions apply to `Projects/Blobbo and Chrono/` and take precedence over repository-wide instructions when they are more specific.

This project is the **actual shipping game**. It owns production gameplay, product-domain state, generated-world architecture, shipping assets, saves, progression, platform adapters, and selected behavior promoted from experiments. It must never depend on `../Blobbo Playground/` code or assets.

Before changing this project:

1. Read the repository-root `AGENTS.md`.
2. Read `PLAN.md` completely. Treat it as the authority for the current product hypothesis, scope, milestone order, acceptance gates, and explicit non-goals.
3. Read `PROJECT_STRUCTURE.md`. Treat it as the authority for project ownership, experiment promotion, automated-test placement, and Nu-native `.nugroup` usage.
4. Read `ARCHITECTURE.md`. Treat it as a description of the system that is actually implemented, not as authority to preserve obsolete prototype assumptions.
5. Inspect relevant experiments under `../Blobbo Playground/` only when the current milestone names them. Never import the playground wholesale.
6. Implement the **current milestone only**. Do not begin a later milestone or fold deferred features into the current change.
7. Prefer typed, deterministic, inspectable data and generators. Media analysis proposes a versioned feature timeline; exact F# generation, validation, physics, and state transitions remain authoritative.
8. Use Nu-native serialized entity trees such as `.nugroup` for authored scene or pattern templates when appropriate, but keep generation keys, pattern contracts, difficulty mapping, validation, runtime mutation, and save semantics in typed versioned systems.
9. Keep every queue, history, cache, generated segment window, recovery buffer, and runtime entity population bounded.
10. Do not introduce YouTube downloading, server-side extraction, hidden capture, redistributed model weights, or a release dependency on a policy- or licence-uncleared adapter.
11. Run the narrow project build first and record exact commands, results, and blockers in `PLAN.md`'s evidence log.
12. Update milestone checkboxes only for work actually completed. Keep human playtest gates pending until human evidence exists.
13. Update `ARCHITECTURE.md` when runtime architecture changes, `PROJECT_STRUCTURE.md` when project/authoring boundaries change, and the `PLAN.md` decision log when evidence resolves or reverses a design choice.

A milestone PR is incomplete without its relevant tests or executable validation, debug/inspection support, documentation updates, and an explicit statement of what was not validated.

Dependency invariants:

- `Blobbo and Chrono` must not reference `Blobbo Playground`.
- Production assets must not load from the playground directory.
- Experimental namespaces, scene addresses, and provider-specific tokens must not enter production save or generation formats.
- A successful experiment is re-owned by production code through an explicit promotion; it is not consumed in place from the playground.
