# Blobbo Playground agent instructions

The playground contains experiments that support the product plan in `../Blobbo and Chrono/PLAN.md`.

Before changing this project:

1. Read the repository-root `AGENTS.md`.
2. Read `../Blobbo and Chrono/PLAN.md` completely and implement only its current milestone.
3. Treat playground scenes as controlled experiments, not automatic production architecture.
4. Preserve the existing baseline behind configuration when comparing bodies, controls, rewind, water, or presentation.
5. Add bounded telemetry, deterministic reset/replay fixtures, and explicit evidence before drawing conclusions.
6. Do not promote an experiment to `../Blobbo and Chrono/` until the relevant code-ready and human gates in the plan are satisfied.
7. Record exact build/test/profile commands and results in the plan's evidence log; do not mark human gates complete without human playtest evidence.

Keep generated binaries, media, captures, caches, model weights, and temporary analysis output out of Git.
