# Blobbo and Chrono agent instructions

These instructions apply to `Projects/Blobbo and Chrono/` and take precedence over repository-wide instructions when they are more specific.

Before changing this project:

1. Read `PLAN.md` completely. Treat it as the authority for the current product hypothesis, scope, milestone order, acceptance gates, and explicit non-goals.
2. Read `ARCHITECTURE.md`. Treat it as a description of the system that is actually implemented, not as authority to preserve obsolete prototype assumptions.
3. Inspect the relevant experiments under `../Blobbo Playground/` when the current milestone names them.
4. Implement the **current milestone only**. Do not begin a later milestone or fold deferred features into the current change.
5. Prefer typed, deterministic, inspectable data and generators. Media analysis proposes a versioned feature timeline; exact F# generation, validation, physics, and state transitions remain authoritative.
6. Keep every queue, history, cache, generated segment window, and recovery buffer bounded.
7. Do not introduce YouTube downloading, server-side extraction, hidden capture, redistributed model weights, or a release dependency on a policy- or licence-uncleared adapter.
8. Run the narrow project build first and record exact commands, results, and blockers in `PLAN.md`'s evidence log.
9. Update milestone checkboxes only for work actually completed. Keep human playtest gates pending until human evidence exists.
10. Update `ARCHITECTURE.md` when runtime architecture changes, and update the `PLAN.md` decision log when evidence resolves or reverses a design choice.

A milestone PR is incomplete without its relevant tests or executable validation, debug/inspection support, documentation updates, and an explicit statement of what was not validated.
