# B7. Repairing Games at Runtime or, How We Learned to Stop Worrying and Love Emergence (Mayet): reading note (full text)

Short answer: a seven-page magazine article that describes a runtime monitor called Mayet. It watches a running game through events, checks them against hand-written rules, and tells the game to fix bad state (for example, push a floating character down). It is shown on one demo game with faults the authors inserted themselves. There is no measurement. It is about repairing a game while players play it, not about editing a game while designing it. It does not use rollback.

## Citation
Lewis, C., & Whitehead, J. (2011). Repairing Games at Runtime or, How We Learned to Stop Worrying and Love Emergence. *IEEE Software, 28*(5), 53-59 (Focus: Engineering Fun), September/October 2011. https://doi.org/10.1109/ms.2011.87 . University of California, Santa Cruz. Demo code said to be open source (BSD-style licence) at zenetproject.com/pages/lakitu (p. 59; not visited).

## Versions
- One version read: the IEEE Xplore publisher PDF, 7 pages, pp. 53-59, downloaded by `litfetch` (stamp "Downloaded on November 01,2026 at 16:27:17 UTC").
- Check date: 2026-11-02.
- Page numbers below are the printed IEEE Software page numbers (53-59). The PDF file pages are these minus 52.

## Access and read depth
Full text via `litfetch`, campus access (status `entitled` in `docs/agentic-gaia/litfetch-optional.json`). I read all text, the sidebar (game failure taxonomy), the rule listings (Fig. 2), the limitations and the reference list. The figures (screenshots of the demo) were not viewed, only their captions and the code listings.

## What the paper is
A system description with a demo, in a practitioner magazine. It is neither a user study nor an experiment. The demo, Lakitu, is a Super Mario World-style game built on the open-source Infinite Mario Bros engine. The authors added faults themselves (too-high jumps, coins worth two points, Bullet Bills spawning too often), instrumented the game, attached Mayet and wrote repairs (pp. 55-58). Sample: one game, three seeded fault types, no counts. I did not check the magazine's peer-review process.

## Key results (with locators)
1. **Idea** (p. 54): you cannot enumerate all outputs of an "emergent" game at test time, so use the actual output at runtime: watch for invalid states and repair them.
2. **Fix the problem, not the cause** (p. 54-55, "Crysis problem"): finding the cause of a failure is hard, but identifying the bad state and repairing it is "comparatively easy". They hypothesise that most problems have many more possible causes than solutions. This is a hypothesis.
3. **Architecture** (pp. 55-58): the game is hand-instrumented to send small immutable event messages through a message queue (ActiveMQ) to a separate rule engine (Drools). The rule engine checks human-written integrity rules and, on a violation, sends a repair message back. **The rule engine never changes the game's state itself.** The game reads repair messages in its own loop and decides when to apply them, for example delaying a repair until it is out of the player's view (p. 58).
4. **Rules** (p. 55, Fig. 2 p. 58): an experienced programmer writes rules in advance to say what can go wrong and how to repair it. They express "safety properties" (something bad never happens). Unbounded "something good must eventually happen" (a liveness property in the model-checking sense) cannot be checked by this dynamic method. If you add a time bound (pick up the key within 30 minutes) it becomes a safety property (p. 55). Note: "liveness" here is not the live-programming sense used elsewhere in this review.
5. **Separation reasons** (pp. 55-56): language independence, implementation independence, and execution independence. The rule engine can run on another thread, core or machine. Asynchronous running means a failure may be visible for a few frames before repair (p. 55).
6. **Repairs implemented** (p. 58): move Mario, add level components, remove enemies. They say the system can monitor, react and repair "all within a few frames" (p. 58). This is a claim with no measurement.
7. **Failure taxonomy** (sidebar, p. 56): nontemporal categories (invalid position, invalid graphical representation, invalid value change, artificial stupidity, bad information, bad actions) and temporal ones (invalid position over time, invalid context state over time, invalid event occurrence over time, interrupted event, implementation response). Mayet cannot yet cover invalid graphical representation or implementation response.
8. **Limitations stated by the authors** (p. 58): it repairs logical state, not rendering bugs. Repair cascades or loops (one repair violates another rule) and repairs that make things worse (accelerating a hovering character into a spike pit) are possible. Mitigation: make minimal repairs and keep rules narrow. They say the only way to know is to scale up to a full game, "which we haven't yet attempted".

## Verbatim quotes (checked against the PDF text; page numbers are printed pages)
1. (p. 55) "Although trying to find the cause of a problem is difficult, identifying the problem and repairing it is comparatively easy."
2. (p. 55) "These rules specify safety properties, which express that something bad doesn’t happen (such as getting stuck in a wall)."
3. (p. 58) "The rule engine communicates via messages. It never actually modifies the SUT’s state."
4. (p. 58) "the only way to truly assert whether this is a problem, and how it can be solved, is to try and scale up to a full game, which we haven’t yet attempted."

("SUT" means system under test, the game.)

## Limitations and how strong the evidence is
- Weak evidence. One demo game, faults inserted by the authors, no counts, no timing, no comparison with other methods, no user study.
- The rules must be written by an experienced programmer who can predict failures; the cost is compared to writing unit tests (p. 55).
- Only logical state; no rendering faults.
- Known risk of repair loops, untested at game scale.
- Written for players of a shipped game ("acceptable failures"), not for a designer editing a game.

## Relevance to the Gaia agent design
- **Runtime checks as bounded safety properties.** The point that "eventually" cannot be tested but "within N frames" can matches the planned `gaia_assert` (a predicate checked each frame for N frames, with the first failing frame reported). Advice for the agent's prompts: write bounded assertions ("the ball stays inside the walls for 300 frames"), not open-ended ones.
- **Check state, not just code.** The authors argue for checking observed state against rules. This supports giving the agent structured world state (`gaia_inspect`, `gaia_hierarchy`) and not only compiler output. It is design reasoning; the paper does not test it.
- **Repair versus rollback.** Mayet does forward repair, not rollback. Gaia's `gaia_undo` and the snapshot taken before each mutation are the opposite design: go back to a known state. Mayet adds a warning for any automatic repair by an agent: repairs can cascade or make things worse, so keep repair steps minimal and cap the number of rounds. This agrees with the design's cap of about three repair rounds, which comes from other sources.
- **The game decides when to apply a repair.** In Mayet the rule engine only proposes; the game enacts. A similar split suits Gaia: the editor thread applies each change and keeps the undo snapshot, and the MCP server only proposes.
- **Hiding bugs.** Repairing symptoms at runtime can hide a real design bug. For a design-time agent, a failed assertion should lead to a fix in the source, not a silent patch of live state.
- **Latency.** The article's "few frames" is an unmeasured claim; do not cite it as a latency figure.
- Not covered: state-preserving reload, alternatives, MCP safety of a code-running tool.

## Corrections to the current literature-review.md entry (B7)
Entry: "Finding (from Z-note): runtime repair of observed game state. Informs: safety (runtime repair/rollback)."
- **"Runtime repair of observed game state" is right.**
- **"rollback" is not supported.** The paper never rolls state back. It sends forward repairs (move, add, remove). Remove "rollback" from the "Informs" line, or say that the paper contrasts with rollback only by implication.
- **Add the type and strength:** magazine system description plus one seeded-fault demo; no measurements.
- **Add the terminology warning:** its "liveness property" is the model-checking term, not live programming.
- Metadata (IEEE Software 28(5), 53-59, DOI 10.1109/ms.2011.87) is correct.
