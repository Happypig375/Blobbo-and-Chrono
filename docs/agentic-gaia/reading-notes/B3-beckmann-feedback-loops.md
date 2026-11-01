# B3. Shortening Feedback Loops in a Live Game Development Environment: reading note (main paper in full; supplement not read)

Short answer: a five-page tool paper. The authors built a Squeak/Smalltalk game environment that keeps the game running while you edit, with state snapshots and input replay. They judge it only by building three small games themselves. There is no user study and no timing data. The useful part for us is the careful treatment of which edits are temporary and which persist, and why editing derived state is ambiguous.

## Citation
Beckmann, T., Krebs, E., Rein, P., Ramson, S., & Hirschfeld, R. (2021). *Shortening Feedback Loops in a Live Game Development Environment.* In 2021 IEEE Symposium on Visual Languages and Human-Centric Computing (VL/HCC), pp. 1-5. https://doi.org/10.1109/VL/HCC51201.2021.9576454 . Hasso Plattner Institute, University of Potsdam (the PDF spells it "Hasso Platter Institute"). Supplement named in footnote 1: https://doi.org/10.5281/zenodo.5082421 (illustrated walkthrough). Code named in footnote 2: https://github.com/hpi-swa-lab/tools-interactive-simulations/ (not visited).

## Versions
- One version read: the IEEE Xplore publisher PDF, 5 pages (4 pages of text plus references on p. 5), downloaded by `litfetch` (stamp "Downloaded on November 01, 2026 at 16:26:37 UTC").
- Not read: the Zenodo supplement (footnote 1) and the GitHub code.
- Check date: 2026-11-02.
- Page numbers below are PDF pages, which equal the printed pages 1-5.

## Access and read depth
Full text of the 5-page paper via `litfetch`, campus access (status `entitled` in `docs/agentic-gaia/litfetch-optional.json`). All text and the reference list were read. Figures 1 and 2 were not viewed (only their captions). **The Zenodo supplement was not read by me.** The review entry says a prior agent read "main paper and supplement in full"; I cannot confirm the supplement part.

## What the paper is
A tool (proof-of-concept) paper. The evaluation is described by the authors as "tentative": they made three small games with the tool (an isometric map renderer, a tower-defence game, a side-scrolling platformer; Sec. IV, p. 4) and describe their own experience. Sample: three prototypes, built by the authors. No participants, no measurements, no comparison to another tool.

## Key results (with locators)
1. **Two design principles** (Sec. III, p. 2): the game runs at all times, and the tools know about the running game.
2. **Architecture** (Sec. III-A, p. 2): an Entity-Component-System (ECS) design, built in Squeak/Smalltalk, which "provides hot code reloading infrastructure". ECS means data is stored in components attached to entities, and "systems" are code that loop over entities with certain components. Tools: component definitions, a query tool whose results can be edited, system code editors, a game view, snapshot and time controls, a profiler (Fig. 1, p. 3).
3. **State snapshots** (Sec. III-B, p. 2): a snapshot is a deep copy of all component data at one moment. The game starts from an initial snapshot (called delta-0). Restart resets the current state to delta-0. A "step" button advances one tick.
4. **Which snapshot does an edit change?** (Sec. III-B, p. 2). This is the main idea.
   - Change only the current state: the edit is temporary and vanishes on restart.
   - Change the initial snapshot too: the edit persists.
   - For a value that does not change by itself (a wall's position), changing both is consistent.
   - For derived state (a patrolling enemy's position at time i), changing both is ambiguous. After a restart the enemy would not be where the user put it, so the live state becomes inconsistent. So persisting such an edit needs an explicit opt-in (a checkbox, or a modifier key).
   - Things created only during play do not exist in the initial snapshot. Their fields are coloured to show they cannot be persisted. "Entity templates" let a designer edit the definition and update all live instances, with the same derived-state drawback.
5. **Replay** (Sec. III-B, pp. 2-3): the user can save a snapshot delta-r of the current state and record external events (keys, mouse, manual spawns). Replay resets to delta-r and re-fires the recorded events. The editor can auto-replay after each code save, which gives fast feedback but may produce a state that is inconsistent with a fresh start, because the code change applies mid-run. For effects that fire once, an "auto-restart" option resets to delta-r (or delta-0) after each save (p. 3).
6. **Editing a changing value** (Sec. III-C, p. 3): the query tool freezes a continuously changing value while you edit it, and shows the live value beside it.
7. **Tools inside the game** (Sec. III-D, p. 3): the authors' working hypothesis is that the line between tooling and game should blur. Debug overlays and gizmos are written as ordinary ECS systems that can be grouped and switched on and off. This is stated as a hypothesis; it is not measured.
8. **Experience reported** (Sec. IV, p. 4): balancing towers and enemies by changing template values and auto-replaying a scenario; debugging A* pathfinding with an overlay, watches and the step button; level design by recording a maximum-length jump and replaying it. They say the environment let them "find suitable, working designs faster than in traditional game engines" (a belief, not a measured comparison). They suggest edit transactions for large refactorings (p. 4).
9. **Other engines** (Sec. V, p. 4): in Godot, changes to initial state override run-time state and game-state changes cannot be persisted; in Unreal, the running game must be interrupted to enter an edit mode. Stated without evidence.

## Verbatim quotes (checked against the PDF text; page numbers are PDF pages)
1. (p. 1, abstract) "We tentatively demonstrate the effectiveness of the live programming features in the context of the development of game prototypes of different genres."
2. (p. 2, Sec. III-B) "A state snapshot is a copy of the game state at a given moment in time."
3. (p. 3, Sec. III-B) "it may result in inconsistent game state as changes to the code are directly reflected in the game behavior"
4. (p. 3, Sec. III-D) "The working hypothesis is that the line between tooling and game needs to blur to allow writing tools with as little cognitive overhead as possible."

## Limitations and how strong the evidence is
- Weak evidence of benefit. The conclusion rests on the authors' own use for three prototypes. They say so: "We found the new mindset around keeping the game running to be beneficial during development" (p. 4, Sec. VI, Conclusion). No numbers on iteration time, no participants, no baseline.
- The environment is a custom Smalltalk ECS tool, not a general engine. The snapshot design relies on being able to deep-copy all state.
- Replay assumes the simulation is deterministic given recorded events. The paper does not say how it handles randomness or time.
- Focus is gameplay code and entity data, not rendering or assets.

## Relevance to the Gaia agent design
- **Temporary versus persistent edits.** This is the most useful lesson. In Gaia, an edit by `gaia_eval` changes the live world (like a change to the current state only). It disappears on restart unless the agent saves it with `gaia_save` or changes the source and runs `gaia_reload_code`. The design already says this (`design.md` §4.3). B3 explains why derived state is ambiguous: do not "persist" a value that the simulation computes (for example a moving ball's position). The agent should persist the rule or the initial value, not the current value.
- **Snapshots, restart, replay.** The paper's snapshot-and-restart loop is close to Gaia's snapshot-before-mutation and `gaia_undo`, plus the designed `gaia_checkpoint` and `gaia_input`/`gaia_assert`. Note that the paper does not offer undo of individual edits; its snapshots are for restart and replay. Calling it "snapshot-and-replay as undo" (the current review) is the review's inference.
- **Replay for balancing.** Auto-replaying a recorded scenario after each change is a good model for an agent verifying a tuning change: record once, replay after each edit, assert on the outcome. This is a design idea from the paper; the paper gives no numbers on how well it works.
- **Latency and liveness.** The paper's goal is short iterations, but it reports no latency values. Do not cite it for a number.
- **Spawned-at-runtime entities** cannot be persisted by editing the instance; the agent must change the code that spawns them. Useful warning for Breakout-style bricks or balls spawned by code.
- Not covered: MCP safety, runtime repair, alternatives.

## Corrections to the current literature-review.md entry (B3)
Entry: "a Squeak/Smalltalk ECS with hot code replacement, snapshots and replay. Distinguishes temporary state edits, persisted edits and replay from a snapshot. Overwriting derived state risks losing history. Co-locating tools with the game is a design hypothesis, not a measured benefit." / "Informs: ... snapshot-and-replay as undo."
- **Correct:** Squeak/Smalltalk ECS; temporary versus persisted edits; replay from a saved snapshot; tools-in-game is stated as a "working hypothesis" (p. 3).
- **Imprecise:** "Overwriting derived state risks losing history." The paper's concern is different: if you apply an edit to derived state to both the current and initial snapshot, the current state becomes inconsistent with what a restart would produce (patrolling enemy example, p. 2). It is about inconsistency, not lost history.
- **Overstated:** "snapshot-and-replay as undo". The paper has no undo feature. Snapshots are for restart and replay. Say "snapshots for restart and replay" instead.
- **Add:** the evidence is the authors' own use on three small games; "tentatively" is the paper's own word. Also note the supplement (Zenodo) was not re-read in this pass.
- The page range "1-5" in the review is right.
