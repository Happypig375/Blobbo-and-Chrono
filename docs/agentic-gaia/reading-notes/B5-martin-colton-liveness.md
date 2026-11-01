# B5. Towards Liveness in Game Development: reading note (full text)

Short answer: this is a four-page design and work-in-progress paper. It has no experiment and no user study. Its value for us is a clear list of why live editing of a running game goes wrong (stale state, temporary edits), plus one concrete "replay the input log after a code change" mechanism and its known weakness.

## Citation
Martin, A. R. (Andrew R. Martin), & Colton, S. (2019). *Towards Liveness in Game Development.* IEEE Conference on Games (CoG 2019), London, 20-23 August 2019, pp. 1-4. https://doi.org/10.1109/cig.2019.8848092 . Affiliations on the PDF: Game AI Research Group, Queen Mary University of London (both authors); S. Colton also SensiLab, Monash University. The venue and page range (CoG 2019, pp. 1-4) are also given in reference [20] of B3 (Beckmann et al. 2021). The PDF itself does not print the conference name; it carries the IEEE copyright line "978-1-7281-1884-0/19/$31.00 ©2019 Crown".

## Versions
- One version read: the IEEE Xplore publisher PDF, 4 pages, as downloaded by `litfetch` (stamp on every page: "Downloaded on November 01, 2026 at 16:25:37 UTC").
- No other version (preprint, author copy) was compared. The review mentions a "conference-hosted copy" per the Zotero note; I did not look for it.
- Check date: 2026-11-02.
- Page numbers below are the PDF page numbers 1-4, which equal the printed pages 1-4 of the paper.

## Access and read depth
Full text via `litfetch`, campus access (status `entitled` in `docs/agentic-gaia/litfetch-optional.json`). I read all four pages and the reference list. Nothing was skimmed. I did not follow any of the cited demos (Victor, Light Table, Dynamo).

## What the paper is
A position and work-in-progress paper. It has three parts: a short survey of four ways to make game development interactive (REPLs and notebooks, the Smalltalk model, dataflow and reactive programming, reactive GUI frameworks; Sec. II, pp. 2-3); a design for a new reactive, node-graph game tool (Sec. III, pp. 3-4); and a tiny prototype (a Tetris-like demo, Fig. 3, p. 4). There is no sample, no measurement, and no evaluation. Evaluation is listed as future work (Sec. IV, p. 4). The Zotero note's "4-page paper fully read" is right.

## Key results (what it claims, with locators)
1. **Core claim.** Immediate feedback ("liveness") in game tools may raise productivity, creativity and enjoyment. This is a belief, not a result (abstract, p. 1: "We believe that...").
2. **REPL edits are temporary.** A REPL embedded in a game is good for looking at state and trying commands. But its changes are usually temporary, so a REPL is poor for authoring (Sec. II-A, p. 2).
3. **Live editing of a running game can give incoherent state.** In the Smalltalk-style model, code changes stay. But changes to dynamic state cannot sensibly be saved, and a code or data change may not reach things that already exist. Their example: change the enemy size variable, and enemies already spawned keep the old size, so the game now holds two sizes that the code can never produce (Sec. II-B, p. 2).
4. **Why it is hard in games.** Web tools (Elm, React) get hot reload with state kept and time-travel debugging from persistent immutable data. Games need fast mutable data, so that route has a performance cost (Intro, p. 1; Sec. III-A, p. 3).
5. **Their mechanism for live code change: replay.** The controller logs every input event. When the programmer edits the game code, it throws away the game object, builds a new one, and feeds the whole logged event history back in a tight loop. So a change to behaviour or initial state shows up across the whole playthrough, in real time (Sec. III-D, p. 4; Fig. 3 shows a changed block shape applied to the whole history).
6. **Known weakness of that mechanism.** A change to rendering code needlessly re-runs the whole event log. They plan to fix this with a dependency graph of nodes (p. 4).
7. **Planned design.** A reactive node graph in which nodes hold mutable state, but mutable references cannot be shared between nodes (to avoid hidden coupling), checked by the language's type system (Sec. III-A, p. 3).

## Verbatim quotes (checked against the PDF text; page numbers are PDF pages)
1. (p. 2, Sec. II-A) "Any modifications made using a REPL are usually temporary."
2. (p. 2, Sec. II-B) "changing a variable which controls the size of enemies in a game may not affect the enemies that have already spawned"
3. (p. 2, Sec. II-B) "the game may now contain enemies of two different sizes, which is not representative of the code and can never happen again."
4. (p. 4, Sec. III-D) "When the programmer changes the code for the game, the controller discards the game component and initialises a new one. It then runs a tight loop providing all previous logged events to the component."

## Limitations and how strong the evidence is
- Weak as evidence. It is a vision paper with one toy demo. It cannot show that liveness helps anyone. Treat it as a source of design vocabulary and failure cases, not of effects.
- The authors' own survey of other tools is short and unsystematic; they say it is not meant to be exhaustive (Sec. II, p. 2).
- The prototype is a purpose-built language and tool, not an existing engine. The replay method assumes the game is a deterministic function of its logged inputs. The paper does not discuss randomness, time, or physics.
- Seven years on, the paper says liveness ideas had not reached game tools (Intro, p. 1). That is the authors' view, not a surveyed fact.

## Relevance to the Gaia agent design
- **Stale state after a change.** The enemy-size example is exactly what an agent will meet in Gaia. If `gaia_eval` or `gaia_reload_code` changes a default, a dispatcher, or a system, entities that already exist may not change. The agent must not conclude "it works" from the old objects. After an edit it should restart from a snapshot or respawn, then check (`gaia_status`, `gaia_inspect`, `gaia_screenshot`). This supports the design's rule that live edits are temporary and a restart is the real test.
- **Temporary versus persistent.** The REPL point (edits are temporary) matches the design's "exploration versus commitment" split in `design.md` §4.3. B3 gives more detail.
- **Replay as a check.** Logging input and replaying it after a change is the same idea as `gaia_input` plus `gaia_advance` plus `gaia_assert` (designed, not all built). This is a design inference from the paper; it is not tested here. Whether Nu's world replays deterministically from the same inputs is not known from this paper and should be checked before relying on it.
- **Latency.** The paper gives no latency numbers. The "re-run the whole log" cost is a warning that a full replay gets slower as the session grows. An agent loop should replay short, named scenarios, not whole sessions.
- Not relevant: runtime repair, branching of alternatives, and MCP safety. The paper is silent on them.

## Corrections to the current literature-review.md entry (B5)
- The entry says "The finding was not extracted by me" and gives no finding. This note supplies it (see Key results). The review's metadata is right: Martin & Colton, IEEE CoG 2019, DOI 10.1109/cig.2019.8848092.
- "Author list differs, see above": the PDF lists exactly two authors, Andrew R. Martin and Simon Colton. The review's citation matches the PDF.
- Suggested wording for "Informs": "Position/WIP paper (4 pp., no evaluation). Shows why live code or data edits can leave stale state in a running game, and proposes event-log replay as the live-update mechanism; its own weakness is replaying the full log."
- No other error found.
