# A3. GameEngineBench: Evaluating Coding Agents on Real C++ Runtime Environments: reading note

## Citation
La, B., Chang, S., Kim, B., Bae, J., Beg, A. A., Chang, S., Gonzalez-Pumariega, G., & Goyal, K. (2026). *GameEngineBench: Evaluating Coding Agents on Real C++ Runtime Environments.* arXiv:2607.03525 [cs.SE]. https://doi.org/10.48550/arxiv.2607.03525 (abs: https://arxiv.org/abs/2607.03525). Affiliations: Nitrode (first three authors), Nexon Intelligence Labs, Dartmouth, Columbia, Cornell, University of Chicago. Venue claim: none (no comment field, none in the PDF). Footnote: views are the authors' own, not their employers'. Repository (per PDF header, not visited): https://github.com/Nitrode-Research/GameEngineBench.

## Versions
- Latest version: v2 (submitted 3 Jul 2026 as v1; v2 dated 15 Jul 2026 03:41:26 UTC; the abs page checked 2026-11-01 lists only v1 and v2).
- Version read: v2 (PDF header "arXiv:2607.03525v2 [cs.SE] 15 Jul 2026"), 17 pp.
- v1 not read; what changed between v1 and v2 is unknown.
- Check date: 2026-11-01.
- Reading note in `~/workspace/research-workflow-evaluation`: none (grep for the arXiv ID and for the title found nothing). Zotero note Z:RJ9GV3AP was not consulted (pointer only).

## Access and read depth
Open access (arXiv). Read depth: full text, all 17 pp., including the appendices (A sample tasks, B supplemental figures, C complementarity examples, D reproducibility notes). Figures 5, 7, 9-13 are charts whose pdftotext output is scrambled (rotated axis labels); only values printed as labels (Fig. 4, 6, 7 matrices, 8, 10) were used and plots were not viewed. Protocol label: `indexed_body_complete` for text; figures partly unreadable.

## What the paper is
A 110-task benchmark of scoped native C++ implementation tasks inside nine open-source Unreal Engine 5 projects (HordeTemplateV2Native, ActionRoguelike, Bomber, TargetVector, GASShooter, Eternal Crusade Resurrection, LASAA, PBMovementBench, NanoGSBench) (Sec. 4). Each task gives a buildable start state, a list of editable C++ files and a public behaviour specification; the test suite is withheld and injected after the solve phase; tests run through Unreal automation in Play-in-Editor listen-server mode (Sec. 3, 5). Reference solutions add a mean of 511 and median of 362 lines in the editable files (Sec. 2). Success is decided by an LLM judge that sees the specification, test source, test results and assertion failures, the model's edits and the reference solution (Sec. 5). Twelve model-harness configurations, one attempt per task, solver timeout 3,600 s, compile and test 600 s each (Table 2).

## Key results (with locators)
1. **Headline (Sec. 6.1, Fig. 4, Fig. 6).** "Calibrated pass@1": Claude Fable 5 (max effort) 55.5%; GPT-5.5 xhigh 29.1%; Claude Opus 4.8 max 23.6%; GPT-5.5 high 19.1%; Gemini 3.1 Pro high 18.2%; Opus 4.8 high 12.7%; Opus 4.7 high 10.0%; GPT-5.5 medium 9.1%; Kimi for Coding 8.2%; Sonnet 4.6 high 7.3%; Qwen 3.7 Plus 3.6%; DeepSeek 4 Pro 2.7%; GLM 5.2 2.7%. The top configuration is almost twice the second; the authors state that part of the spread "partially reflect[s] wrapper effects rather than pure model capability" (Sec. 7).
2. **Reasoning effort (Sec. 6.1).** GPT-5.5: medium 9.1%, high 19.1%, xhigh 29.1%.
3. **Resource use (App. Fig. 6).** Mean model cost per task / median solve time / mean output tokens: Fable 5 max $17.2 / 17.9 min / 78k; GPT-5.5 xhigh $9.9 / 13.4 min / 30k; Opus 4.8 max $10.8 / 25.1 min / 116k; Gemini 3.1 Pro $3.3 / 6.6 min / 11k; DeepSeek 4 Pro $1.4 / 7.6 min / 26k. Authors note cost/token accounting is incomplete for some wrappers.
4. **Coverage and complementarity (Sec. 6.3-6.4, Fig. 7-9, 11, App. C).** 79/110 tasks are solved by at least one configuration; 31 by none; none by all. Task 15 (AI controller + behaviour tree; initialisation order, blackboard observer, client RPC) is solved only by GPT-5.5 xhigh; Task 19 (generated map orchestrator; pooling, replication, ready signal) only by Fable 5 max. Per-area solved-by-any counts (Fig. 8): movement 3/3, data/serialization 3/3, online 2/2, XR 2/2, gameplay ability 4/5, core gameplay 15/19, animation/camera/locomotion 11/14, AI/world 3/4, inventory 11/15, UI/sessions 9/13, engine systems/pooling 8/12, graphics plugins 4/7, save/persistence 2/5, weapons/combat 2/6. The text says save/persistence "have the largest unresolved share", but by the Fig. 8 labels weapons/combat (4 of 6 unsolved) is higher than save/persistence (3 of 5); small internal inconsistency, categories are tiny (authors: "directional evidence").
5. **Failure mechanisms (Sec. 6.5, Fig. 5).** Outcomes are decomposed into calibrated pass, behaviour/test fail, judge-disputed, compile fail. "Higher-performing wrappers usually reach compilation and execution; their remaining errors are mostly behavioral rather than purely syntactic." Recurrent patterns: missing server authority, client state not updated (OnRep hook without the replicated source state), missing local-controller checks, missing constructor defaults / activation timing / teardown, initialisation order, object lifecycle; "tasks that remain unsolved ... share a common failure mode: cross-system coordination".
6. **Task calibration pipeline (Sec. 3, Fig. 1).** Test validation (start project builds, reference passes, start state fails intended behaviours) plus an LLM-judge review of a strong model's attempt to find under-specified, too-narrow or over-constrained tests, iterated ("task calibration").
7. **Judge role (Sec. 5, Sec. 7).** A run counts as success "when the LLM judge determines that the generated implementation satisfies the requested behavior"; the judge can convert test failures into passes and the authors admit "LLM review can convert some test failures into valid solutions, showing that the tests do not always capture the full behavior space". The judge model is not named in the text; "the evaluation protocol supports cross-family judging whenever multiple model families are available" to mitigate self-judging bias.
8. **Compiler use (App. Fig. 12).** Mean Unreal compiler invocations per task and compile errors per task are plotted for wrapper-visible trajectories; the plotted values were not reliably extractable and are not used here.

## Verbatim quotes
1. (p. 6, Sec. 5) "A run is counted as successful when the LLM judge determines that the generated implementation satisfies the requested behavior."
2. (p. 9, Sec. 6.5) "The higher-performing wrappers usually reach compilation and execution; their remaining errors are mostly behavioral rather than purely syntactic."
3. (p. 8, Sec. 6.4) "These tasks are difficult for agents because correctness requires coordination between several runtime systems, rather than the use of any single Unreal API call."
4. (p. 10, Sec. 7) "LLM review can convert some test failures into valid solutions, showing that the tests do not always capture the full behavior space of these tasks."
5. (p. 10, Sec. 7) "because the evaluated models are run through different wrappers, the observed pass@1 differences partially reflect wrapper effects rather than pure model capability."
6. (p. 7, Sec. 6.1) "Increasing reasoning effort from medium to high to xhigh raises pass@1 from 9.1% to 19.1% to 29.1%."

## Corrections to the current literature-review.md entry (A3)
Entry: "110 scoped C++ tasks in Unreal Engine 5 projects. Changes must compile and pass behavioural tests. Best of 12 configurations reaches 55.5% pass@1, and 31 tasks are unsolved by all. Informs: evaluation; engine-API difficulty." Depth: "abstract (WF)".
- "Changes must compile and pass behavioural tests" is not how success is defined. Success is an LLM-judge verdict ("calibrated pass@1") after a compile and an injected PIE test run; the judge may override test failures. Figure 5 has a separate "judge-disputed" category. This matters: the benchmark's headline number is not a deterministic-test result and the judge model is unnamed. Suggest: "compile + injected PIE tests, final verdict by an LLM judge that can overturn test failures".
- "Best of 12 configurations reaches 55.5%": correct, but 55.5% is one configuration (Claude Fable 5 at max effort) and the next best is 29.1%; there are 13 rows in Fig. 4 (12 configurations are claimed in the abstract; the listed rows are Fable 5 max, GPT-5.5 x3 efforts, Opus 4.8 x2, Opus 4.7, Sonnet 4.6, Gemini 3.1 Pro, Kimi, Qwen, DeepSeek, GLM = 13). Minor internal count mismatch in the paper; I counted 13 labelled configurations.
- Add the version: v2 (15 Jul 2026), not v1.
- Missing: wrapper confound (different CLI harness per model), one attempt per task, reasoning effort effect (9.1 -> 29.1% for GPT-5.5).
- Missing for "engine-API difficulty": the paper attributes difficulty to cross-system runtime behaviour (authority, replication, object lifecycle, initialisation order), not to API knowledge.
- Missing: no visual feedback of any kind appears in this paper (the only "screenshots" are illustrations of reference solutions in Fig. 2). It is relevant to evaluation and failure modes, not to the visual-feedback question.

## Design relevance
- **Behaviour failures dominate once code compiles.** The best wrappers get through compilation and still fail on runtime behaviour. For Gaia this supports giving the agent a way to observe the consequence of a change after time advances (state after N ticks), not just "eval succeeded". No visual evidence here.
- **Cross-system coordination is the hard part.** The unsolved tasks involve several systems that must agree (ordering of initialisation, state replication, cleanup). In a live editor the analogue is changes that interact with other entities/dispatchers; a world-diff or invariant check after each edit targets the same failure class.
- **Withheld tests plus scoped editable files.** Hiding the tests during solving and restricting which files can change is a cheap hygiene pattern that Gaia evaluations should copy (and A6 does the same with fresh-project replay).
- **Judge-over-tests is a trade-off.** The authors needed an LLM judge because tests miss valid implementations. In Gaia, prefer state assertions plus a human-readable diff for the cases the assertions cannot cover, and report judge-overridden cases separately (Figure 5's "judge-disputed" is a good model).
- **Cost scales with success here.** The best configuration costs $17.2 per task and 17.9 median minutes; budget the number of editor-feedback rounds accordingly.

## Limits / credibility
- Preprint (v2), no venue claim; credibility-gate label: `context_or_prior_art_only`.
- Industry authors (Nitrode, Nexon) with an open repository; no conflict statement beyond the "views are my own" footnote.
- One attempt per task and model; wrapper effects confound the model comparison; unequal cost/token accounting across wrappers; the LLM judge is unnamed and its agreement with human reviewers is not reported in the text I read.
- Open-source Unreal projects only; gameplay-facing C++ only (no audio, performance, editor tooling, platform, security); PIE listen-server rather than production topologies (Sec. 7).
- No visual feedback component; results do not bear on screenshots or video.
- Task area subgroups are very small (2 to 19 tasks), so area-level differences are "directional".
