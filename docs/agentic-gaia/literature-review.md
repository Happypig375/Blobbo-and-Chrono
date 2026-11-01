# Literature review: agentic, feedback-driven game design in an engine editor

Date: 2026-11-01 (HKT). Scoping review for the Gaia agent-interface design, not a systematic review.

## Method and provenance (read this before relying on any entry)
- **Workflow:** follows `~/workspace/research-workflow-evaluation` (AGENTS.md, docs/protocol.md §1–8,
  docs/source-credibility.md). The applicable rules are extracted in `literature-protocol.md` (read-depth labels,
  provenance fields, discovery log, credibility gate, operational vs saturation stops).
- **Who read what:** searching and first-pass reading were done by a Sonnet subagent (Scite + web + Zotero
  export); I (Opus, synthesising) did not re-read sources except the spot-check below. Read depths are as recorded
  by that agent; "abstract (WF)" means a WebFetch small-model paraphrase of an abstract page, i.e. *not verbatim*.
- **Tools:** Scite worked (~17 `search_literature` calls, limit 8–15, page one only; no `read_fulltext`,
  no `citation_graph`). **Consensus was unavailable**: its monthly quota (500 searches) was exhausted
  ("resets on November 1st"), so the Consensus branch is *blocked*, an operational stop, not saturation (see
  NEEDS-HUMAN.md). No `litfetch` runs were made (lower priority than other projects); closed papers are listed
  at the end. Zotero was read only via the nightly JSON export; items marked `Z:<key>` rest on notes written by
  earlier agents (secondary, a pointer, not our reading).
- **Credibility:** most sources are 2026 arXiv preprints → `context_or_prior_art_only` / `not_yet_assessed` under the
  credibility gate; venue claims from arXiv comments are unverified. Benchmark results are benchmark-bounded.
- **Spot-check by me (A1, GameDevBench, the most load-bearing source):** Scite metadata + full-text excerpts for
  `10.48550/arxiv.2602.11103` (depth: `excerpts`). Confirmed: 132 tasks (v1), best agent 54.5%, 46.9% → 31.6%
  gameplay vs 2D graphics, "two simple image and video-based feedback mechanisms … consistently improve
  performance", largest change Claude Sonnet 4.5 33.3% → 47.7%, and the Editor Screenshot MCP design. **Nuance the
  first pass missed:** a results excerpt says Claude Sonnet 4.5 "improves from 33.3% to 41.7% using video while
  seeing no improvement when using the MCP", and "using both MCP and video provides negligible additional
  benefit". So editor screenshots alone did not help every model; runtime motion evidence did. This directly
  motivates the frame-sequence tool in `design.md`. (Which configuration produced 47.7% was not in the excerpts.)

- **Round 2 (2026-11-01, afternoon):**
  - **Full-text reading.** Thirteen load-bearing sources were read by two Sonnet/xhigh readers, following
    research-workflow-evaluation's D8 rule (Sonnet/xhigh for coverage of key results). Twelve were read in full
    text from the latest arXiv versions. B2's retrieval *failed*, which is distinct from closed access. Each has
    a reading note in `reading-notes/` with versions, read depth, locators and verbatim quotes. The entries carry
    a "Round 2 full-text reading" bullet, which supersedes earlier claims where they conflict.
  - **Quote check.** I re-downloaded A1 (v1 and v2) and C14 and searched the PDFs for their quoted strings
    (results in the "Quote verification" section: all 14 genuine). Other notes are unverified at quote level.
  - **Consensus.** The quota reset on 1 Nov and three searches ran (section H, abstract depth). Every
    Scite/Consensus call is logged in `~/workspace/exchange/literature-quota.tsv`.
  - **Search ledger and Zotero status.** All searches are in `search-ledger.tsv`. Per-entry Zotero status is in
    `zotero-status.tsv`, matched by DOI or arXiv id against the read-only nightly export: 15 of 76 entries are in
    Zotero, and the rest are labelled "not yet in Zotero" (NEEDS-HUMAN item 4). Title-prefix matches were
    checked by hand, and four false ones were rejected.

---

## A. LLM / agentic game development, PCG

**A1. GameDevBench.** Chi, W., Fang, Y., Yayavaram, A., et al. (2026). GameDevBench: Evaluating agentic capabilities through game development. *arXiv:2602.11103* (ICML 2026, PMLR 306, per Z-note). https://doi.org/10.48550/arxiv.2602.11103
- Route: Scite + Zotero (Z:62B3T52G). Depth: abstract (Scite, v1) + 5 excerpts + Z-note (v2/PMLR).
- Finding: Godot tasks from tutorials. Agents solve about half (v1: 54.5% of 132; Z-note v2: 53.8% of 333). Success drops from 46.9% on gameplay tasks to 31.6% on 2D graphics. Two simple feedback tools consistently improve performance. One is an Editor Screenshot MCP that returns an editor screenshot (scene, node tree, inspector). The other is runtime video. Claude Sonnet 4.5 rose from 33.3% to 47.7%.
- Informs: visual feedback; the editor-screenshot-as-MCP-tool pattern, which is closest to our design. Limit: tutorial-derived localized edits.
- Spot-check (Opus, Scite excerpts): for Claude Sonnet 4.5, runtime video 33.3% → 41.7%, screenshot MCP alone no improvement, both combined ≈ best single method. See the provenance section above.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/A1-gamedevbench.md`). Depth: full text v2 (ICML 2026, PMLR 306) + v1 Sec. 4.2. The entry mixes versions: 132 tasks, 46.9%/31.6% and 33.3→47.7 are v1; v2 (333 tasks) gives 51.4%/33.0%, GPT-5.4 41.1→52.0, Sonnet 4.5 28.8→34.8 (both arms). The open spot-check question is resolved: in v1 Table 2, 47.7% is Sonnet 4.5 with **video only**; the prose's 41.7% is a v1 text/table mismatch. Video-only +14.4, screenshot-only 0.0, both +12.9. In v2, 'both' is below video-only for all three models with isolated arms ("video runtime often provides more improvement than enabling both", v2 p. 7). Agents solve by code generation alone (no editor GUI).

**A2. GameCraft-Bench.** Luo, T. D., Wang, R., Bi, J., et al. (2026). GameCraft-Bench: Can agents build playable games end-to-end in a real game engine? *arXiv:2606.17861*. https://doi.org/10.48550/arxiv.2606.17861
- Route: Scite. Depth: excerpts (5; the abstract is only a figure caption).
- Finding: 140 Godot tasks. Agents must deliver replayable demonstration traces. A multimodal judge scores sampled gameplay frames (2 fps) against hidden rubrics. Mechanics scores exceed content and presentation scores. The authors argue many failures "only become visible after rendering" and that without visual feedback "an agent may continue editing code while remaining blind" (a claim, not an ablation).
- Informs: visual feedback; judge-based evaluation.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/A2-gamecraft-bench.md`). Depth: full text v1. The screenshot claim rests on a call count and one case study, not an ablation; screenshot use does not track score (GPT-5.5 1.91 calls/task → 39.49; Kimi 21.41 → 30.65). Much low-score mass is protocol non-compliance (no valid replay traces). Judge is GPT-5.5, also an evaluated agent; human calibration is a small check (judge +3.32 points more permissive). Best 41.46% (Opus 4.7). Transferable design: agent-written, deterministically replayable input traces as evidence.

**A3. GameEngineBench.** La, B., Chang, S., Kim, B., et al. (2026). GameEngineBench: Evaluating coding agents on real C++ runtime environments. *arXiv:2607.03525*. https://doi.org/10.48550/arxiv.2607.03525
- Route: Zotero (Z:RJ9GV3AP) + WebFetch. Depth: abstract (WF).
- Finding: 110 scoped C++ tasks in Unreal Engine 5 projects. Changes must compile and pass behavioural tests. Best of 12 configurations reaches 55.5% pass@1, and 31 tasks are unsolved by all.
- Informs: evaluation; engine-API difficulty.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/A3-gameenginebench.md`). Depth: full text v2 (15 Jul 2026). Success is **not** 'compile and pass behavioural tests': an unnamed LLM judge gives the verdict after compile + injected PIE run and can override test failures. 55.5% is one configuration (Claude Fable 5, max effort); next best 29.1%. Difficulty is cross-system runtime behaviour (replication, lifecycle, init order), not API knowledge. No visual feedback in this paper. Harness confounded with model.

**A4. GameLogicBench.** Che, X., Ge, Y., Li, S., et al. (2026). GameLogicBench: Evaluating coding agents on runtime game logic with tick-level state assertions. *arXiv:2609.21562* (v2). https://doi.org/10.48550/arxiv.2609.21562
- Route: Zotero (Z:XMYGZCHB) + WebFetch. Depth: abstract (WF) + Z-note (prior agent reports a complete reading).
- Finding: 72 Godot logic tasks, checked per tick with 1,451 seeded cases. Best of 20 combinations solves 52.78%. Most failed submissions run but implement the behaviour incorrectly. The evaluator caught faulty submissions only after validation against mutants. Agents copied public code when the network was open.
- Informs: why "it runs" is not feedback enough; state-assertion oracles; evaluation hygiene (network isolation).

**A5. GameXpert-Bench.** Chen, K., Hong, H., Gao, P., et al. (2026). GameXpert-Bench: How far are coding agents from expert game development? *arXiv:2608.21833*. https://doi.org/10.48550/arxiv.2608.21833
- Route: Scite + WebFetch. Depth: abstract (WF; Scite abstract is a figure caption).
- Finding: three tracks (create, fix, multi-turn human-guided optimisation; GameOpt has 17 chains of 6 turns). Agents build playable foundations but struggle to find defects, verify runtime behaviour and preserve existing functionality as changes accumulate.
- Informs: iterative design evaluation; regression across turns (undo/checkpoints).

**A6. CraftBench-UE.** Wu, S., Calderone, K., Tsen, A., et al. (2026). CraftBench-UE: Deterministic evaluation for coding agents in Unreal Engine. *arXiv:2609.23142*. https://doi.org/10.48550/arxiv.2609.23142
- Route: Scite. Depth: abstract + 5 excerpts.
- Finding: 70 tasks (C++, Blueprint, editor scripting) with deterministic build, asset and runtime checks and no LLM judge. Compared two editor-tool (MCP) configurations. C++ completion exceeds Blueprint by 30.0 and 42.9 percentage points on 10 paired tasks. 42.2% and 50.0% of asset-passing Blueprint submissions fail runtime assertions. In trajectories, "success revisions do not guarantee correct gameplay". They exclude creative tasks needing visual judgement. They note MCP-Unity and Epic's in-editor Unreal MCP as existing authoring tool sets.
- Informs: tool-interface design; why structure-valid does not mean behaviour-valid; limits of LLM-free grading for creative work.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/A6-craftbench-ue.md`). Depth: full text. 42.2%/50.0% are on-time submissions (19/45, 20/40) on the 10 paired tasks; counting late ones 34.3%/40.0% of all final submissions. MCP1 vs MCP2 differ in entry points (3 vs 176), permissions and drivers, so no clean tool-granularity evidence (authors say so). **Conflict of interest:** the authors' organisation develops MCP2. What it supports: readback/inspection evidence matters more than write success; agents rarely run or look at the game unless pushed (image capture in ~6–9% of BP runs).

**A7. AutoUE.** Yin, L., Cheng, W., Qin, Z., et al. (2026). AutoUE: Automated generation of 3D games in Unreal Engine via multi-agent systems. *arXiv:2603.07106*. https://doi.org/10.48550/arxiv.2603.07106
- Route: Scite + WebFetch. Depth: abstract (WF).
- Finding: multi-agent pipeline (assets, scenes, code, automated playtest). Retrieved engine documentation is supplied to reduce tool-use errors, and a runtime playtest pipeline is added. The abstract gives no numbers.
- Informs: docs retrieval for engine APIs; an editor-feedback analogue.

**A8. GUI Agents for Continual Game Generation (Play2Code / PlaytestArena).** Huang, Y., Li, B., Li, N., et al. (2026). *arXiv:2605.28258* (v1 27 May; v2 2 Oct 2026). https://doi.org/10.48550/arxiv.2605.28258
- Route: Scite + Zotero (Z:28BG5ATZ, Z:IIE3UT2H) + WebFetch. Depth: abstract (WF) + Z-notes (prior agent reports all 19 pages of v2 read). Preprint.
- Finding: 200 browser-game tasks. A builder agent and a playtester that cannot see the rubrics iterate via shared memory. Play2Code reaches a 66.8% rubric pass rate, beating single-pass and agentic-coding baselines by 37.1 and 14.6 points. Scores rise across rounds. A separate judge does the scoring.
- Informs: GUI/playtest-in-the-loop feedback; separating the builder from the verifier.

**A9. GameGen-Verifier.** Jia, C., Wan, R., Sun, T., et al. (2026). *arXiv:2605.07442*. https://doi.org/10.48550/arxiv.2605.07442
- Route: Scite + WebFetch. Depth: abstract (WF).
- Finding: verifies LLM-generated games via keypoints plus runtime state injection (force a target state, bounded interaction, check). Up to 92.2% agreement with human judgements versus 58.8% for the open-ended-play baseline, and up to 16.6x faster.
- Informs: state-injection oracles. This maps naturally onto world inspection and REPL eval in a live editor.

**A10. RSIGame.** Wu, W., Fu, M., You, J., et al. (2026). RSIGame: Autonomous agentic game development with recursive self-improvement. *arXiv:2609.39045*. https://doi.org/10.48550/arxiv.2609.39045
- Route: Scite + WebFetch. Depth: abstract (WF).
- Finding: naive iterative refinement overfits a small set of tests. A local explore-diagnose-improve loop plus a global loop that keeps the best checkpoint and detects regression improves quality under matched budgets on GameCraft-Bench tasks.
- Informs: checkpoint/best-so-far retention; overfitting to test cases.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/A10-rsigame.md`). Depth: main text + appendices A–E, F.1 (F.2–F.3 skimmed). Overfitting is the authors' framing, not a controlled test. Budgets matched on rounds/tool calls, approximately on tokens. Judge is Qwen3.8-27B (also the play agent and a generator), so scores are not comparable with A2. The best checkpoint is chosen by a separate Monitor with its own proxy rubric, isolated from the benchmark rubric. Key unmentioned result: replay-based verification catches 76.2% of failed edits versus 0% for build-only checks.

**A11. CreativeGame.** Ma, H., Wang, H., Wang, S., et al. (2026). *arXiv:2604.19926*. https://doi.org/10.48550/arxiv.2604.19926
- Route: Scite + WebFetch. Depth: abstract (WF).
- Finding: single-shot generation is brittle. Subjective creativity scores make poor optimisation signals, so the authors use programmatic proxy rewards, runtime validation and lineage memory (a 71-lineage case study).
- Informs: evaluating "creative" design iteration with proxies.

**A12. OpenGameEval (Roblox Studio).** Zhang, T., Ayyar, K., Sun, M., & Gong, L. (2025, Dec 17). Using OpenGameEval to benchmark agentic AI assistants for Roblox Studio. Roblox Engineering blog. https://about.roblox.com/newsroom/2025/12/opengameeval-benchmark-agentic-ai-assistants-roblox-studio
- Route: Zotero (Z:ZE9IAQ5W). Depth: Z-note only (prior agent: article and bounded repo reconstruction). A second description, "evaluates Roblox Studio tasks with executable tests", is in the A6 excerpts. Practitioner article, not peer-reviewed.
- Informs: an in-editor assistant evaluated via executable client/server tests (closest industry analogue to Gaia).

**A13. LLMaker: function calling for game content.** Gallotta, R., Liapis, A., & Yannakakis, G. N. (2024). Consistent game content creation via function calling for large language models. *IEEE CoG 2024*, 1-4. https://doi.org/10.1109/cog60054.2024.10645599
- Route: Scite (PCG query). Depth: 5 excerpts (no abstract).
- Finding: designer-to-LLM natural language, with the LLM issuing function calls to a back-end that edits the level directly. Function calling never triggered parser or domain failures (though design failures were still possible) and responded in under 10 s. Baselines that returned JSON or used CoT failed within 1-2 responses. Limits: author-compiled test cases (4-page paper).
- Informs: API-mediated edits over regenerated artifacts. It shows structured calls are robust and live-compatible, and that domain/design validity is still separate.

**A14. PCG survey with LLMs.** Maleki, M. F., & Zhao, R. (2024). Procedural content generation in games: A survey with insights on emerging LLM integration. *Proc. AAAI AIIDE, 20*(1), 167-178. https://doi.org/10.1609/aiide.v20i1.31877
- Route: Scite. Depth: abstract (full text institutional-only).
- Finding: surveys search-based, ML, noise and LLM PCG, and identifies gaps. Background only.

**A15. PCG Benchmark.** Khalifa, A., Gallotta, R., et al. (2025). The Procedural Content Generation Benchmark: An open-source testbed for generative challenges in games. ACM (venue not verified). https://doi.org/10.1145/3723498.3723794
- Route: Scite. Depth: abstract. Finding: 12 game-related PCG problems with variants for evaluating generators. Informs: reusable PCG evaluation tasks. (LLM coverage not checked.)

**A16. Lottery and Sprint Arcade.** Torii, M. G., Murakami, T., & Ochiai, Y. (2026). *arXiv:2607.10711*. https://doi.org/10.48550/arxiv.2607.10711
- Route: Scite + WebFetch. Depth: abstract (WF).
- Finding: voice-driven, play-time game editing via an LLM mapping to about 100 tunable settings. User study with self-report, workload and logs. Experiences were mostly favourable. Changes to noticeable settings related to higher usability, and changes to core structure related to enjoyment. Sample size not read.
- Informs: human-study designs for LLM game editing.

**A17. LLMs in game development, autoethnography.** Johnson, K., Ahmed, M., Lang, C., et al. (2026). *arXiv:2603.27896*. https://doi.org/10.48550/arxiv.2603.27896
- Route: Scite + WebFetch. Depth: abstract (WF).
- Finding: collaborative autoethnography of two projects with embedded LLMs found more variability and personalisation but problems with correctness, difficulty tuning and structural coherence. It studies LLMs as game components, not as developer tools, so it is of limited relevance.

---

## B. Live programming, REPL and exploratory programming

**B1. Beyond Code Generation (Pail).** Zamfirescu-Pereira, J. D., Jun, E., Terry, M., Yang, Q., & Hartmann, B. (2025). *CHI '25*, 1-17. https://doi.org/10.1145/3706598.3714154 (arXiv:2503.06911)
- Route: Scite + WebSearch + WebFetch. Depth: abstract (WF); ACM page returned 403; Scite full text denied. A search snippet of the HTML version mentions a lab study with 11 participants (unverified).
- Finding: code LLMs hand back one point solution. An IDE that surfaces alternatives and tracks and flags design decisions let participants explore more of the design space. Participants struggled to keep up with LLM-made code changes and were overloaded by information.
- Informs: exploratory design iteration and attention management (what the agent surfaces to the designer).

**B2. Liveness in the Age of Agents.** Mattis, T., Taeumel, M., Böhme, L., & Hirschfeld, R. (2026). PX/26 workshop at ‹Programming› 2026 (talk abstract). https://2026.programming-conference.org/details/px-2026-papers/12/Liveness-in-the-Age-of-Agents-Are-we-Back-to-Compile-and-Run-Cycles-
- Route: WebSearch. Depth: abstract (WF). Workshop abstract only; no study design visible.
- Finding: uses Tanimoto's liveness levels and Norman's gulfs. Argues agents can bring back slow compile-and-run cycles and that the gulf of evaluation may overwhelm novices. Position-level evidence.
- Informs: why a REPL/world-inspection agent should keep feedback latency low and reviewable.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/B2-liveness-agents.md`). Depth: abstract only (verbatim, conference page); PDF retrieval **failed** (ACM DL 403 Cloudflare; proceedings PDF link 404), not closed (OpenAlex: CC BY). Not a talk abstract: a 7-page full paper (pp. 71–77), doi 10.1145/3801119.3801131. Needs a human download or litfetch.

**B3. Shortening feedback loops in a live game development environment.** Beckmann, T., Krebs, E., Rein, P., Ramson, S., & Hirschfeld, R. (2021). *IEEE VL/HCC*, 1-5. https://doi.org/10.1109/vl/hcc51201.2021.9576454 (Z:WWPCM2VC)
- Route: Zotero + Scite. Depth: Scite metadata (closed) + Z-note (prior agent: main paper and supplement read in full). Short paper.
- Finding (from Z-note): a Squeak/Smalltalk ECS with hot code replacement, snapshots and replay. Distinguishes temporary state edits, persisted edits and replay from a snapshot. Overwriting derived state risks losing history. Co-locating tools with the game is a design hypothesis, not a measured benefit.
- Informs: REPL granularity; edit semantics (temporary vs persistent); snapshot-and-replay as undo. Relevant to Nu's live world editing.

**B4. Live-tooling in the game industry.** Beckmann, T., Flach, C., Krebs, E., Ramson, S., Rein, P., & Hirschfeld, R. (2019). An exploratory literature study on live-tooling in the game industry. *LIVE 2019* (author preprint). (Z:8RJSNJML; no DOI verified)
- Depth: Z-note only. Finding (from Z-note): practitioner-literature crawl (Gamasutra/GDC Vault) of live game tooling. Informs: what live tools game developers actually use.

**B5. Towards Liveness in Game Development.** Martin, A. R., & Colton, S. (2019). *IEEE CoG 2019*. https://doi.org/10.1109/cig.2019.8848092 (Z:Z98DZFSU)
- Depth: Scite metadata (closed; author list differs, see above) + Z-note (prior agent: 4-page paper fully read). Paywalled but important; a conference-hosted copy exists per the Z-note.
- Informs: liveness for game dev. The finding was not extracted by me.

**B6. State Explorer.** Volkovas, R., Fairbank, M., Woodward, J. R., & Lucas, S. (2020). Practical game design tool: State Explorer. *IEEE CoG 2020*. https://doi.org/10.1109/COG47356.2020.9231863 (Z:C8K9CW2D)
- Depth: Z-note only. Finding (from Z-note): a Godot editor plugin that explores reachable states under designer-supplied predicates. It found an unintended move in Tic-Tac-Toe and supplied undo for Sokoban without game-side undo logic. Used on a commercial tower-defence game, with substantial integration cost reported.
- Informs: state-copy/exploration as editor tooling; design-space queries an agent could issue.

**B7. Repairing games at runtime (Mayet).** Lewis, C., & Whitehead, J. (2011). *IEEE Software, 28*(5), 53-59. https://doi.org/10.1109/ms.2011.87 (Z:88QVA7LU)
- Depth: Scite metadata (closed) + Z-note. Finding (from Z-note): runtime repair of observed game state. Informs: safety (runtime repair/rollback).

**B8. SuBViS.** MacCormick, D., & Zaman, L. (2019). *FDG 2019*, 1-11. https://doi.org/10.1145/3337722.3337740 (Z:BDIULBB9)
- Depth: Scite metadata (closed) + Z-note. Finding (from Z-note): a Blueprint tool with node-level and graph-level "alternatives". Switching alternatives did not change behaviour while the game was running and required a manual recompile. No runtime state transfer. Informs: branching/alternatives for design exploration, and the limits of non-live alternatives.

**B9. The essence of live coding.** Bärenz, M. (2020). *SPLASH Onward!*, 2-14. https://doi.org/10.1145/3427763.3428312 (Z:WTCS6U8P)
- Depth: Scite metadata only (closed). Title thesis: "change the program, keep the state". Not read. Informs: state-preserving hot reload semantics.

**B10. SpaceTime Programming.** Döderlein, J.-B., Khelladi, D. E., & Acher, M. (2026). *arXiv:2603.18735*. https://doi.org/10.48550/arxiv.2603.18735
- Route: Scite. Depth: abstract + 5 excerpts.
- Finding: unifies live, exploratory and omniscient debugging by tracing code changes and execution. A Pygame demo. Overhead 35-150% on test suites, and line-level monitoring was about 40x slower. Proof of concept; no user study.
- Informs: trace/replay as agent-readable history.

**B11. ContextBranch.** Nanjundappa, B. C., & Maaheshwari, S. (2025). Context branching for LLM conversations. *arXiv:2512.13914*. https://doi.org/10.48550/arxiv.2512.13914
- Route: Scite. Depth: abstract + 5 excerpts.
- Finding: checkpoint/branch/switch/inject over conversation state. On 30 synthetic scenarios, quality was 2.5% higher (d=0.73), context was 58% smaller. The authors note that there was no study with real developers.
- Informs: branching context for exploration. Weak evidence.

**B12. REPL-Plan.** Liu, A. Z., Wang, X., Sansom, J., et al. (2024). Interactive and expressive code-augmented planning with large language models. *arXiv:2411.13826*. https://doi.org/10.48550/arxiv.2411.13826
- Route: WebSearch + WebFetch. Depth: abstract (WF; no numbers given there).
- Finding: LLM executes and checks code step by step in a REPL (planning domains, not game editors), correcting errors as they arise.
- Informs: REPL as the agent's action loop.

**B13. From Tool Calling to Symbolic Thinking.** de la Torre, J. (2025). LLMs in a persistent Lisp metaprogramming loop. *arXiv:2506.10021*. https://doi.org/10.48550/arxiv.2506.10021
- Route: WebSearch + WebFetch. Depth: abstract (WF). Design framework, no empirical evaluation.
- Finding: persistent live Lisp REPL where models define and revise their own tools. Informs: persistent REPL state for tool-building.

**B14. Fsge: an experimental F# game engine for 2D games.** Kesselman, J. P. (2025). SSRN. https://doi.org/10.2139/ssrn.5362782 (Z:V5RFB2HU)
- Depth: metadata only (open access but not read). Informs: the F# game-engine context. SSRN preprint.

---

## C. Execution-feedback and agent-interface loops

**C1. ReAct.** Yao, S., Zhao, J., Yu, D., et al. (2023). *ICLR 2023* (per arXiv comment). arXiv:2210.03629. https://doi.org/10.48550/arxiv.2210.03629
- Route: Scite + WebFetch. Depth: abstract (WF). Finding: interleaving reasoning and acting reduces error propagation and hallucination relative to CoT-only in QA and interactive tasks. Informs: think-act-observe loop.

**C2. Reflexion.** Shinn, N., Cassano, F., Berman, E., et al. (2023). *NeurIPS 2023*. arXiv:2303.11366. https://doi.org/10.48550/arxiv.2303.11366
- Route: Scite + WebFetch. Depth: abstract (WF). Finding: verbal self-reflection stored in episodic memory; 91% pass@1 on HumanEval (as reported). Informs: carrying lessons across attempts.

**C3. Teaching LLMs to Self-Debug.** Chen, X., Lin, M., Schärli, N., & Zhou, D. (2023). arXiv:2304.05128. https://doi.org/10.48550/arxiv.2304.05128
- Route: Scite + WebFetch. Depth: abstract (WF). Finding: the model debugs its own code using execution results plus its own code explanation, with no human feedback. Matches or beats baselines that sample more than 10x as many candidates. Informs: execution feedback; rubber-duck explanation.

**C4. Is Self-Repair a Silver Bullet?** Olausson, T. X., Inala, J. P., Wang, C., Gao, J., & Solar-Lezama, A. (2024). *ICLR 2024*. arXiv:2306.09896. https://doi.org/10.48550/arxiv.2306.09896
- Route: Scite + WebFetch. Depth: abstract (WF). Finding: once repair cost is counted, gains are modest and inconsistent. The bottleneck is the quality of the model's feedback on its own code. A stronger feedback model, and especially human feedback, helps substantially. Informs: external/stronger feedback beats self-critique.

**C5. CRITIC.** Gou, Z., Shao, Z., Gong, Y., et al. (2024). *ICLR 2024*. arXiv:2305.11738. https://doi.org/10.48550/arxiv.2305.11738 (Route: WebFetch only.)
- Depth: abstract (WF). Finding: tool-interactive critiquing; the authors stress "the crucial importance of external feedback". Informs: grounded verification.

**C6. LLMs cannot self-correct reasoning yet.** Huang, J., Chen, X., Mishra, S., et al. (2024). *ICLR 2024*. arXiv:2310.01798. https://doi.org/10.48550/arxiv.2310.01798 (WebFetch only.)
- Depth: abstract (WF). Finding: without external feedback, intrinsic self-correction often fails and can degrade performance (reasoning tasks, not code).
- Informs: use real signals (eval, inspection, screenshots), not self-review.

**C7. SWE-agent (ACI).** Yang, J., Jimenez, C. E., Wettig, A., et al. (2024). *NeurIPS 2024*. arXiv:2405.15793. https://doi.org/10.48550/arxiv.2405.15793
- Route: Scite + WebFetch. Depth: abstract (WF); Scite excerpts returned none. Finding: interfaces designed for LM agents ("agent-computer interface") improve file editing, navigation and test running. 12.5% on SWE-bench and 87.7% on HumanEvalFix. The paper analyses how interface design affects behaviour. The ablation details were not read.
- Informs: ACI design.

**C8. CodeAct.** Wang, X., Chen, Y., Yuan, L., et al. (2024). Executable code actions elicit better LLM agents. *ICML 2024*. arXiv:2402.01030. https://doi.org/10.48550/arxiv.2402.01030
- Route: Scite + WebFetch. Depth: abstract (WF). Finding: executable Python as the unified action space, up to 20% higher success than JSON/text actions across 17 LLMs, with multi-turn revision via an interpreter. Informs: REPL-eval as the primary action.

**C9. OpenHands.** Wang, X., Li, B., Song, Y., et al. (2025). *ICLR 2025*. arXiv:2407.16741. https://doi.org/10.48550/arxiv.2407.16741
- Depth: abstract (WF). Finding: open platform with sandboxed code execution, shell and browser. Informs: harness reference.

**C10. SWE-bench.** Jimenez, C. E., Yang, J., Wettig, A., et al. (2024). *ICLR 2024*. arXiv:2310.06770. https://doi.org/10.48550/arxiv.2310.06770
- Depth: abstract (WF). Finding: 2,294 real issues requiring multi-file edits and execution interaction. Informs: evaluation baseline for code agents (not games).

**C11. Voyager.** Wang, G., Xie, Y., Jiang, Y., et al. (2023). arXiv:2305.16291. https://doi.org/10.48550/arxiv.2305.16291
- Depth: abstract (WF). Finding: Minecraft agent that writes code skills, refines them with environment feedback, execution errors and self-verification, and keeps a skill library. 3.3x more unique items than prior methods. Informs: skill library; combined feedback channels.

**C12. Agent-centric Debugging Interface (ADI).** Xiang, J., Xu, X., Chu, X., Tian, H., & Zhang, Y. (2026). *FSE 2026* (per arXiv). arXiv:2604.24212. https://doi.org/10.48550/arxiv.2604.24212 (Z:B6V8URKN)
- Depth: abstract (WF + Zotero abstract). Finding: a function-level debugging interface with a Frame Lifetime Trace replaces line-by-line debugger stepping, which is too costly for agents. 63.8% on SWE-bench Verified at about USD 1.28 per task, and +6.2% to +18.5% when added to existing agents.
- Informs: richer, summarised runtime state as a tool, with cost-aware granularity. Debugger granularity maps to REPL-step granularity.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/C12-adi.md`). Depth: full text (22 pp.). Published as PACMSE (FSE) 2026, doi 10.1145/3797126; actual title 'Empowering Autonomous Debugging Agents with Efficient Dynamic Analysis'. +6.2% to +18.5% are relative; absolute +2.4 to +5.6 pp. On the same agent a plain PDB tool adds +0.8 pp for +$0.60/task while function-level ADI adds +8.8 pp for +$0.26: inspection granularity matters more than having a debugger.

**C13. Reassessing one-round test-time refinement.** Kim, J. (2026). *AI* (MDPI) 7(9), 353. https://doi.org/10.3390/ai7090353
- Route: Scite. Depth: abstract. Venue/credibility not checked (`not_yet_assessed`).
- Finding: 7 models x 3 Python benchmarks. Self-Debug (execution-based) gave a positive gain in 16 of 21 cells and none negative. Self-Refine (no execution) reduced correctness in 16 cells. Self-Debug preserved 99.95% of already-correct candidates.
- Informs: execution grounding avoids regressions.

**C14. How many tries?** Arimbur, J. J. (2026). *arXiv:2604.10508*. https://doi.org/10.48550/arxiv.2604.10508
- Route: Scite. Depth: abstract. Preprint.
- Finding: feeding errors back up to 5 times gains +4.9 to +17.1 pp (HumanEval) and +16 to +30 pp (MBPP), mostly in the first two rounds. Logic (assertion) errors are the hardest, at about 45% repaired.
- Informs: diminishing returns; error type matters.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/C14-how-many-tries.md`). Depth: full text (11 pp.). 'Up to five attempts' = at most 4 feedback rounds. The ~45% assertion-error repair rate is HumanEval only (~63% on MBPP). Two repair rounds capture most of the gain; specific error messages repair far more often than bare wrong-result signals.

**C15. Feedback Over Form.** McAndrews, C. J. (2026). *arXiv:2604.21950*. https://doi.org/10.48550/arxiv.2604.21950
- Route: Scite. Depth: abstract. Preprint, 1-3B models.
- Finding: execution feedback mattered more than pipeline topology. Refinement fixes runtime errors but rarely logic errors. Early stopping was essential ("without it, every iteration is net-negative").
- Informs: stopping rules; logic bugs need semantic oracles.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/C15-feedback-over-form.md`). Depth: full text (19 pp.). 'Early stopping' means returning as soon as tests pass, not an iteration cap; without it the refiner breaks 14–62% of already-correct code. Design rule: refine only when an explicit check fails.

**C16. Transferability of code repair to low-resource PLs.** Wong, K., Amayuelas, A., Pan, L., & Wang, W. Y. (2025). *Findings of NAACL 2025*, 3410-3432. https://aclanthology.org/2025.findings-naacl.190/ (doi 10.18653/v1/2025.findings-naacl.190)
- Route: Scite + WebFetch. Depth: abstract (WF).
- Finding: benefits of repair distillation depend on the language, and the link between reasoning ability and code-correction ability is weak. Gains in low-resource settings are inconsistent.
- Informs: caution for F# (a lower-resource language); evidence of F# repair loops is thin.

**C17. MultiPL-T.** Cassano, F., Gouwar, J., Lucchetti, F., et al. (2023/2024). Knowledge transfer from high-resource to low-resource programming languages for code LLMs. *arXiv:2308.09895*. https://doi.org/10.48550/arxiv.2308.09895
- Depth: abstract (WF). Finding: code LLMs are weaker on OCaml/Racket and similar languages. Test-validated translation data helps (fine-tuning). Informs: F#/OCaml-family languages are plausibly under-resourced. No F# result was read.

---

## D. Visual / multimodal feedback

**D1. Coding with Eyes (VF-Coder / InteractGUI Bench).** Liu, Z., Huang, Y., Xie, T., et al. (2026). *arXiv:2604.19750*. https://doi.org/10.48550/arxiv.2604.19750
- Route: Scite + WebFetch. Depth: abstract (Scite and WF). Preprint.
- Finding: text-only feedback is weak for event-driven, visual GUI programs. A multi-agent system that interacts with the running UI and inspects visuals raised Gemini-3-Flash's success from 21.68% to 28.29% (visual score 0.428 to 0.558). Modest absolute gain.
- Informs: visual plus interaction feedback.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/D1-coding-with-eyes.md`). Depth: full text. The +6.61-point gain is not all visual: a non-visual version of the same pipeline reaches +3.11 (like text-only CLI agents); vision adds +3.50 inside the pipeline, of which +2.85 comes from screenshot-driven *exploration to find bugs* and only +0.65 from showing the fixer the failing screenshot. Cost ~14× single-shot. Visual score is a ResNet similarity model without human validation.

**D2. Vision-guided iterative refinement (frontend).** Sansford, H., Law, D. H. C., Liu, W., et al. (2026). *arXiv:2604.05839*. https://doi.org/10.48550/arxiv.2604.05839
- Route: Scite + WebFetch. Depth: abstract (WF). Preprint.
- Finding: a VLM critic on rendered pages guides revisions, with gains up to 17.8% over three cycles on WebDev Arena requests. LoRA distilled about 25% of the loop's gains.
- Informs: render, critique, revise loops; cycle count.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/D2-vision-guided-refinement.md`). Depth: full text. 17.8% is best-of-{initial, cycles 1–3} chosen and scored by the same VLM judge for the weakest generator; a fixed 3-cycle loop gives +5.7 to +8.7%. 63–80% of the cycle-3 gain arrives in cycle 1 while tokens grow ~linearly. No visual-vs-text critique ablation. Workshop paper (ICLR 2026 RSI workshop). Static single page: does not speak to runtime/frame feedback.

**D3. UI2Code^N.** Yang, Z., Hong, W., et al. (2025). *arXiv:2511.08195*. https://doi.org/10.48550/arxiv.2511.08195
- Route: Scite. Depth: abstract. Finding: argues single-turn UI-to-code underuses iterative visual feedback; an interactive paradigm with a trained VLM and test-time scaling over multi-turn feedback. Informs: multi-turn visual refinement.

**D4. Perceptual self-reflection (physics simulation code).** Shende, P., & Camburn, B. (2026). *arXiv:2602.12311*. https://doi.org/10.48550/arxiv.2602.12311
- Route: Scite. Depth: abstract. Preprint.
- Finding: a VLM validating rendered animation frames closes the "oracle gap" where code runs but behaves wrongly. Improves over single-shot generation at about USD 0.20 per animation (author-reported).
- Informs: perceptual oracle for simulations (analogous to game visuals).

**D5. VisualWebArena.** Koh, J. Y., Lo, R., Jang, L., et al. (2024). *ACL 2024*. arXiv:2401.13649. https://doi.org/10.48550/arxiv.2401.13649
- Depth: abstract (WF). Finding: benchmark for visually grounded web tasks; text-only agents are limited and multimodal agents still show gaps. Informs: vision matters for UI tasks.

**D6. Design2Code.** Si, C., Zhang, Y., Li, R., et al. (2025). *NAACL 2025*. arXiv:2403.03163. https://doi.org/10.48550/arxiv.2403.03163
- Depth: abstract (WF). Finding: 484 real pages; MLLMs mainly struggle to reproduce visual elements and layout. Informs: VLMs' weaknesses in fine layout.

**D7. OSWorld.** Xie, T., Zhang, D., Chen, J., et al. (2024). arXiv:2404.07972. https://doi.org/10.48550/arxiv.2404.07972
- Depth: abstract (WF). Finding: 369 real-computer tasks; humans about 72%, the best model 12.24% at publication, mainly due to GUI grounding. Informs: screenshot-only GUI control is hard, which favours API access.

**D8. WebArena.** Zhou, S., Xu, F. F., Zhu, H., et al. (2023/2024). arXiv:2307.13854. https://doi.org/10.48550/arxiv.2307.13854
- Depth: abstract (WF). Finding: best GPT-4 agent 14.41% vs 78.24% human on functional-completion tasks. Informs: the general agent-evaluation template (outcome-based checking).

---

## E. MCP and tool-interface design

**E1. MCP specification, Tools (2025-06-18).** Model Context Protocol. https://modelcontextprotocol.io/specification/2025-06-18/server/tools
- Depth: full page read (WF returned the page text). Primary documentation, not research.
- Finding: tools have name, description, inputSchema and optional outputSchema and annotations. Results can include text, image, audio, resource links and structured content. Failures are reported as `isError`. Spec text says there SHOULD be a human in the loop with ability to deny invocations. Clients should validate results and log usage, and servers should validate inputs and rate-limit.
- Informs: image results (screenshots) are first-class; error semantics; the safety guidance.

**E2. Writing effective tools for agents.** Aizawa, K., et al. (2025, Sep 11). Anthropic Engineering. https://www.anthropic.com/engineering/writing-tools-for-agents
- Depth: abstract-level summary (WF). Practitioner article.
- Finding (recommendations): a few high-impact tools rather than many thin API wrappers, consolidating multi-step operations, paginating/truncating responses, writing actionable error messages, and using eval tasks with verifiable outcomes plus transcript review.
- Informs: ACI; tool-eval methodology. Evidence is experiential.

**E3. MCP tool descriptions are smelly.** Hasan, M. M., Li, H., Rajbahadur, G. K., Adams, B., & Hassan, A. E. (2026). *ACM TOSEM* (doi 10.1145/3858625; arXiv:2602.14878). https://doi.org/10.48550/arxiv.2602.14878
- Route: Scite + WebFetch. Depth: abstract (WF).
- Finding: 97.1% of 856 tools across 103 servers have at least one description smell. Augmenting descriptions raised task success by a median of 5.85 pp. It also increased execution steps by 67.46%, and performance fell in 16.67% of cases.
- Informs: tool-description quality is measurable, but "more description" has costs.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/E3-mcp-tool-descriptions.md`). Depth: arXiv v3 full text (52 pp.); TOSEM version not read. TOSEM venue confirmed via Crossref only. 5.85 pp and 67.46% are medians over four model-level changes; only Qwen3-Next has paired baseline/augmented runs (steps −26%). No single best set of description components; dropping Examples did not significantly hurt.

**E4. MCP landscape and security.** Hou, X., Zhao, Y., Wang, S., et al. (2025/2026). *ACM TOSEM 35*(10). https://doi.org/10.1145/3796519 (arXiv:2503.23278)
- Route: Scite. Depth: abstract only; the full text is institutional/closed. Paywalled-but-important.
- Finding: lifecycle and threat taxonomy (16 threat scenarios, four attacker types) with safeguards. Informs: safety of an eval-capable MCP server.

**E5. Learning to rewrite tool descriptions (Trace-Free+).** Guo, R., Dong, K., Gao, X., & Das, K. (2026). *arXiv:2602.20426*. https://doi.org/10.48550/arxiv.2602.20426
- Route: Scite + WebFetch. Depth: abstract (WF). Preprint.
- Finding: interface quality limits agent success, and descriptions degrade as the catalogue grows. Rewriting improved success on StableToolBench (+60.89% query-level success; degradation at catalogues above 150 tools cut by 29.23%).
- Informs: keep the tool set small; refine descriptions from traces.

---

## F. Safety, undo, state management

**F1. Planarian (statepoints).** Guo, J., Chen, H. M., Vaswani, K., Paverd, A., & Pietzuch, P. (2026). *arXiv:2609.35366*. https://doi.org/10.48550/arxiv.2609.35366
- Route: Scite + WebFetch. Depth: abstract (WF). Preprint.
- Finding: statepoints (snapshot, rollback, fork) across local and remote state. Authors report up to 15x task quality gains and 3% overhead for recovery.
- Informs: undo/branching for agent edits.
- **Round 2 full-text reading** (Sonnet/xhigh reader, 2026-11-01; `reading-notes/F1-planarian.md`). Depth: full text (14 pp.). '15×' is Mario only; the other two games gain 1.4×, and a no-rollback control already captures part of the gain. 1–3% overhead is for replayed traces; remote-state rollback covers SQL only. Design-relevant: keep the record of failed attempts across a rollback rather than rewinding the agent's context.

**F2. Fault-tolerant sandboxing.** Yan, B. (2025). *arXiv:2512.12806*. https://doi.org/10.48550/arxiv.2512.12806
- Route: Scite + WebFetch. Depth: abstract (WF). Preprint.
- Finding: policy-filtered commands plus filesystem snapshots as transactions, with 14.5% overhead per transaction. Informs: transactional edits.

**F3. SABER.** Hu, Q., Tang, Y., Wang, Q., et al. (2026). *arXiv:2606.01317*. https://doi.org/10.48550/arxiv.2606.01317
- Route: Scite + WebFetch. Depth: abstract (WF). Preprint.
- Finding: judges coding-agent safety by final workspace state after action sequences. Even the best model had above 54% harmful safety-violation rate. Informs: stateful-workspace risk, so snapshot before destructive eval.

---

## G. Evaluation methods

**G1. A survey on LLM-as-a-judge.** Gu, J., Jiang, X., et al. (2026). *The Innovation*. https://doi.org/10.1016/j.xinn.2025.101253
- Route: Scite. Depth: metadata only (no abstract returned). Informs: judge reliability background; not read.

**G2. SWE-Together.** Wu, et al. (2026). Evaluating coding agents in interactive user sessions. *arXiv:2606.29957*. https://doi.org/10.48550/arxiv.2606.29957 (Z:VNKB29NI)
- Depth: Zotero metadata + note header (prior agent: 14 pages read). Finding not extracted by me. Informs: evaluating agents in interactive sessions with simulated users.

(Also seen as comparators, not separately annotated: Harness-Bench Z:L28XYA6F, a Z-noted agent-harness benchmark; Perish or Flourish Z:KLVL5BID on LLM code generation in functional languages, metadata only, abstract absent from the export. Neither was read.)

---

## H. Consensus branch (round 2, 2026-11-01; abstract depth)

Three Consensus `search` calls (page 0, default page size; 20 + 19 + 20 records, all screened from the inline
page; see `search-ledger.tsv`). Read depth for every entry below: **abstract (Consensus record, provider text)**,
not full text; credibility `not_yet_assessed`. Records already in A–G (GameDevBench-adjacent GameXpert-Bench A5,
CreativeGame A11, REPL-Plan B12, Self-Debug C3, Olausson C4, How many tries C14) were not re-listed.

**H1. WebGen-Agent.** Lu, Z., et al. (2025). WebGen-Agent: Enhancing interactive website generation with multi-level feedback and step-level reinforcement learning. *arXiv:2509.22644*. https://doi.org/10.48550/arxiv.2509.22644
- Finding (abstract): a VLM turns screenshots and GUI-agent testing into text feedback plus scores; "integrated with a backtracking and select-best mechanism"; Claude-3.5-Sonnet accuracy 26.4% → 51.9% on WebGen-Bench.
- Informs: screenshot feedback works best as one scored signal with backtracking to the best-so-far state (our snapshot/undo).

**H2. OpenGame / OpenGame-Bench.** Jiang, Y., et al. (2026). OpenGame: Open agentic coding for games. *arXiv:2604.18394*. https://doi.org/10.48550/arxiv.2604.18394
- Finding (abstract): web-game agent with a "Debug Skill that maintains a living protocol of verified fixes"; evaluation scores Build Health, Visual Usability and Intent Alignment "via headless browser execution and VLM judging" on 150 prompts.
- Informs: headless execution + VLM judging as an evaluation recipe; a persistent fix log is a cheap analogue for Gaia sessions.

**H3. GameWorld.** Ouyang, M., et al. (2026). GameWorld: Towards standardized and verifiable evaluation of multimodal game agents. *arXiv:2604.07429*. https://doi.org/10.48550/arxiv.2604.07429
- Finding (abstract): 34 browser games, 170 tasks with "state-verifiable metrics"; compares raw keyboard/mouse computer-use agents with a semantic action space.
- Informs: state-verifiable checks over pixel judging; a semantic action layer (our typed tools) versus raw GUI control. Game *playing*, not design.

**H4. Is three the magic number?** Kiecker, T., et al. (2026). Is three the magic number? An empirical evaluation of LLM-based repair loops. *ASE 2026* (per record). https://doi.org/10.1145/3832783.3834547
- Finding (abstract): across code generation, test generation and translation, "the first three to four repair iterations account for most achievable gains"; repair behaviour depends more on "workflow orchestration and feedback design than by the underlying model".
- Informs: an iteration cap of ~3–4 per design step, and treat the cap as an experimental variable (design.md §7). Peer-reviewed venue (unverified).

**H5. FeedbackEval.** Dai, D., et al. (2025). FeedbackEval: A benchmark for evaluating LLMs in feedback-driven code repair tasks. *arXiv:2504.06939*. https://doi.org/10.48550/arxiv.2504.06939
- Finding (abstract): mixed feedback best (63.6%), test feedback 57.9%, compiler feedback 49.2%; "marginal benefit diminishes after two or three iterations".
- Informs: return several feedback kinds together (diagnostics + runtime result + state/visual), not compiler errors alone.

**H6. Debugging Decay Index.** Adnan, M., et al. (2025). Measuring and mitigating debugging effectiveness decay in code language models. *Scientific Reports*. https://doi.org/10.1038/s41598-025-27846-5
- Finding (abstract): "most models lose 60-80% of their debugging capability within just 2-3 attempts"; a "strategic fresh start" rescues effectiveness.
- Informs: after a few failed rounds, roll back to the snapshot and restart the step rather than patch further. Peer-reviewed.

**H7. TraceCoder.** Huang, J., et al. (2026). TraceCoder: A trace-driven multi-agent framework for automated debugging of LLM-generated code. *ICSE 2026*. https://doi.org/10.1145/3744916.3773187
- Finding (abstract): runtime-trace probes plus a "Rollback Mechanism [that] enforces that each repair iteration constitutes a strict improvement"; up to 34.43% relative Pass@1 gain.
- Informs: keep best-so-far and roll back non-improving edits (supports A10 and our undo design).

**H8. InspectCoder.** Wang, Y., et al. (2026). InspectCoder: Dynamic analysis-driven self repair through interactive LLM-debugger collaboration. *PACMPL*. https://doi.org/10.1145/3798238 (preprint arXiv:2510.18327)
- Finding (abstract): agent-controlled breakpoints and "incremental runtime experimentation within stateful debugger sessions"; 5.10–60.37% relative repair gains.
- Informs: a stateful interactive session (like Gaia's live FSI) is a better repair substrate than fixed logs; complements C12.

**H9. Inducing code change in self-debugging.** Kim, J. (2026). *AI* (MDPI) 7(10), 392. https://doi.org/10.3390/ai7100392
- Finding (abstract): revision requests left 42.6% of failing programs unchanged; giving "the first reported failure instead of only a failure status increased repair by 12.93–13.43 points"; gains per turn decrease sharply.
- Informs: return the concrete failure (exception text, diagnostic with location), never a bare fail flag. Same author as C13.

**H10. A-ProS.** Tabassum, A., et al. (2026). A-ProS: Towards reliable autonomous programming through multi-model feedback. *ACM TOSEM*. https://doi.org/10.1145/3820891
- Finding (abstract): "stateful refinement outperforms stateless approaches by 8.5–10.6 percentage points and reduces repeated failures by up to 3.5×".
- Informs: keep per-session history (Gaia log, eval transcript) visible to the agent across rounds.

Also seen, not listed (off-target for this design): ScreenAgent, CogAgent (GUI control models); LLM game-agent surveys
(Hu et al. 2024; Gallotta et al. 2024, *IEEE ToG*, a possible background citation); V-GameGym; GameGPT; LiveCodeBench;
code-generation benchmarks without an interactive loop. No Consensus hit addressed an LLM agent inside a
game-editor REPL, consistent with the Scite branch.

## Paywalled or closed, important (not fetched)

- Hou et al. MCP landscape (TOSEM): full text institutional.
- Zamfirescu-Pereira et al. CHI'25: the ACM page returned 403; the arXiv version (2503.06911) is open and the abstract was read.
- Martin & Colton (CoG 2019), Lewis & Whitehead (IEEE Software 2011), Beckmann et al. (VL/HCC 2021, closed on Scite; author copies exist per Z-note), MacCormick & Zaman (FDG 2019), Bärenz (2020), Volkovas et al. (CoG 2020). All have Zotero entries, so a user-side retrieval is possible. None was fetched by me.
- Perish or Flourish (functional programming) Z:KLVL5BID and Harness-Bench Z:L28XYA6F: not read.

---

## Quote verification (round 2, Opus)

Sample: every verbatim quote in two notes, A1 (7 quotes) and C14 (7). Method: re-downloaded the arXiv PDFs (A1 v1
and v2, C14), ran `pdftotext`, normalised whitespace, ligatures and dashes, then did an exact substring search.

| Note | Quotes | Exact match | Genuine across a column break | Not found |
|---|---|---|---|---|
| A1 | 7 | 7 | 0 | 0 |
| C14 | 7 | 5 | 2 | 0 |

- **C14:** the two non-exact quotes are present in full, interleaved with text from the neighbouring column
  (two-column layout).
- **A1, quote 2 locator:** the note labels it v2. It matches v1 exactly and appears in v2 split by the same layout
  effect, so the locator stands.
- **Scope:** this checks quotation fidelity only. As research-workflow-evaluation found, a quote check does not
  catch claim errors. The other 11 notes are unverified at quote level.

## Synthesis: design implications (evidence-graded)

**Execution feedback.** Where the oracle is real execution, loops help (C3, C8, C11). Round-2 reading and the
Consensus branch converge on four points:
- Most of the gain comes in the first one to three rounds (C14 full text, D2, H4, H5, H6).
- Refine only when an explicit check fails; refining code that already passes breaks 14–62% of it (C15).
- Concrete failure text beats a bare fail flag (C14, H9).
- Roll back non-improving attempts, but keep the record of what failed (F1, H7, A10). Gains concentrate in the first two or three rounds and mostly fix runtime errors, not logic errors (C14, C15). Self-review without grounding is unreliable or harmful (C4, C6, C13; C5 supports external feedback). Implication: end every design step with an external observation (eval result, world diff, screenshot), cap iterations, and keep a best-so-far checkpoint (A10). Evidence is mostly Python benchmarks. F#/Nu is untested, and repair in lower-resource languages is inconsistent (C16, C17).

**Visual feedback.** Full-text reading sharpens this conclusion:
- **Runtime evidence beats editor screenshots.** In GameDevBench (A1), runtime *video* carried the gain, not
  editor screenshots. In v1 for Sonnet 4.5: video only +14.4 points, screenshot only 0.0, both +12.9. In v2,
  "both" is below video-only for every model with isolated arms.
- **Screenshots help find bugs more than fix them.** In VF-Coder (D1), screenshots mainly drove exploration that
  found bugs (+2.85 points). Showing the fixer the failing screenshot added only +0.65.
- **Agents rarely look unless pushed, and write success is not correctness.** Agents rarely look at the running
  game unless pushed, and a successful write does not imply correct gameplay (A2, A6).
- **Replay catches what builds miss.** In A10, replay-based verification caught 76.2% of failed edits; build-only
  checks caught 0%.
- **Render-critique gains come early.** In D2, render-critique gains concentrate in cycle 1.

Screenshot-only control remains hard (D7, D8). Implication for Gaia:
- Prefer runtime evidence: frame sequences after advancing time (`gaia_frames`), replayable input (`gaia_input`,
  next) and state assertions (`gaia_assert`, next).
- Use editor screenshots for exploration and detection rather than as the main verification channel.

No study ablates screenshots against structured inspection in a live editor; design.md §7 proposes that ablation.

**REPL granularity and liveness.** No LLM-agent-in-game-editor-REPL study was found; REPL-Plan and the Lisp REPL (B12, B13) are planning or design-level. Debugger-granularity evidence (C12) favours function-level summarised traces over line stepping. Live-game work (B3, B6, B8, B10) says edit semantics must be explicit (temporary vs persisted state; snapshot/replay). Agent latency can erode liveness (B2). Implication: small eval calls for probes, larger scripted transactions for edits.

**Tool interface (ACI).** Inspection granularity matters more than raw access. In ADI (C12, full text), a
function-level dynamic-analysis tool added +8.8 pp at +$0.26 per task, where a plain debugger added +0.8 pp at
+$0.60. E3 (full text) found no single best tool-description template. C7, E2, E3, E5 and A6 converge: few task-shaped tools, structured results, actionable errors, concise descriptions, and images as first-class MCP content (E1). Longer descriptions cost steps (E3). A successful write does not imply correct gameplay (A6). Only SWE-agent is peer-reviewed; the rest are preprints or practitioner pieces.

**Safety and undo.** Snapshot/rollback and transactional execution are supported by early preprints (F1, F2). Workspace safety is poor even for frontier models (F3). Game-specific precedents: B6, B7, B8, B11. The MCP spec recommends a human in the loop (E1). Implication: wrap each design transaction in a world snapshot plus a replayable script. Evidence that this improves design quality is thin.

**Evaluation.** Benchmarks use deterministic tests (A4, A6), judges (A2, A8) or state injection (A9). Documented hazards: overfitting to tests (A10), unvalidated evaluators (A4), cross-turn regression (A5), network leakage (A4), and LLM-free grading excluding creative tasks (A6). Human evidence is weakest (B1, A16 are small studies; no working-designer study of agent-in-editor design). Implication: combine state assertions, independently-informed judges and a small within-subject study (time to goal, rollbacks, perceived control, attention load).

**Thin or missing.** Consensus ran only in round 2, with three abstract-depth queries. B2's full text could not be retrieved. Nothing was found on F#/Nu/Gaia specifically. Most sources are 2026 arXiv preprints not credibility-checked. WebFetch entries are paraphrases. `citation_graph` and Scite pagination were not used (open frontier).
