# B8. SuBViS: The Use of Subjunctive Visual Programming Environments for Exploring Alternatives in Game Development: reading note (full text)

Short answer: a tool paper with a use-case walkthrough and a very small user study (four people). SuBViS adds "alternatives" (named versions) to Unreal Engine's Blueprint visual scripting, at the level of one node or the whole graph. Users liked being able to try ideas without fear of losing earlier work. But switching alternatives did not change the behaviour of a running game, and it did not trigger a recompile. So alternatives were a design-time, edit-and-restart feature, not a live one.

## Citation
MacCormick, D., & Zaman, L. (2019). SuBViS: The Use of Subjunctive Visual Programming Environments for Exploring Alternatives in Game Development. In *Proceedings of Foundations of Digital Games (FDG '19)*, San Luis Obispo, CA, 26-30 August 2019, 11 pages. ACM. https://doi.org/10.1145/3337722.3337740 . University of Ontario Institute of Technology (Ontario Tech University), Canada. Built on Unreal Engine's Blueprint Visual Scripting by modifying the engine source (Sec. 3, p. 4).

## Versions
- One version read: the ACM Digital Library publisher PDF, 11 pages, downloaded by `litfetch` (`docs/agentic-gaia/litfetch-optional.json`, status `entitled`).
- Check date: 2026-11-02.
- Page numbers below are PDF pages 1-11, which match the ACM page range.

## Access and read depth
Full text via `litfetch`, campus access. I read all 11 pages, including the study procedure, results and future work. Figures (graph screenshots, the Likert chart in Fig. 10) were not viewed; I used the text only. The Likert values themselves are therefore not quoted: the paper's text says only that SuBViS was rated equal or higher in all four categories (p. 8). The first part of the reference list (to [14]) was read; the rest was skimmed.

## What the paper is
A tool paper with (a) a hypothetical use-case example (p. 4-6) and (b) "a small-scale user study" (Sec. 4-5, pp. 7-10). The study is a small within-subject comparison of base Blueprints against SuBViS. The authors state its goals as getting first impressions and finding out how to run a later, larger study (p. 7). Sample: four unpaid male participants, ages 20-25, from technical programmes, with strong coding experience and little Unreal experience (mean 2.0 on a 1-7 scale) (Sec. 4.1, p. 7).

## Key results (with locators)
1. **What SuBViS is** (Sec. 3, p. 4): two levels of alternatives. A *node alternative* stores several states of one node (for example three colours). A *graph alternative* stores a whole copy of the graph, which you can rewire freely. The two combine. Alternatives are independent copies. A browser-like tab UI switches between them (p. 4).
2. **How it is run** (p. 4): when you compile the Blueprint, "the currently active combination" of alternatives is used. Because Unreal compiles only the active graph, many alternatives do not slow compilation.
3. **Use case** (Sec. 3.1, pp. 4-6): a shooting-range target moves in a circle, then alternates direction, then moves in a square, and has three hit colours. The authors argue that doing this in plain Blueprints means duplicating the Blueprint (breaks Unreal's type casts) or putting all variants in one graph with branches (unreadable). This is an argument, not a measurement.
4. **Study design** (Sec. 4.3, pp. 7-8): two tasks (target, crate), one with each system, with order and task balanced across four participants. Task 1: make three alternative behaviours as instructed. Task 2: make three of your own. Think-aloud, notes, screen recording, a 1-7 Likert questionnaire (efficiency, ease of use, likelihood of future use, overall), and a semi-structured interview. Sessions averaged about 1.5 hours.
5. **Ratings** (Sec. 5, p. 8): "The participants ranked SuBViS equal or higher than the base system in all categories." No statistics are reported; the authors call the small sample "an important caveat".
6. **What users did** (Sec. 5.1-5.3, pp. 8-9): node alternatives were used little (one parameter in one node) and were seen as useful only for nodes with many parameters. Participants found the tab UI mostly self-explanatory (two had brief initial confusion over one button). In base Unreal they joined all variants into one graph or used branches; one said the only way would be "something hacky". One participant likened SuBViS to Git and said it let them use "trial and error safely" (p. 9).
7. **Main limitation found: not live** (Sec. 5.3, p. 9). Participant P2 made two behaviours, ran the game, switched alternatives, and nothing changed in play. The nodes changed in the editor, but the running behaviour stayed the same. In the authors' words, behaviours cannot be switched at runtime; the user must stop and restart. Also, switching alternatives did not tell Unreal to recompile, so every participant had to recompile by hand at least once and some got confused.
8. **No side-by-side runtime view** (p. 9-10): comparing two alternatives in play needs a switch each time. Ideal future work: run several game instances in parallel and view them together (Sec. 7, p. 10).
9. **Authors' own plans** (Sec. 5.4, 7, p. 10): a longitudinal study with experienced users, a larger sample, naming alternatives, a choice between copy and blank for new alternatives, and function support.

## Verbatim quotes (checked against the PDF text; page numbers are PDF pages)
1. (p. 9, Sec. 5.3) "Firstly, behaviours cannot be switched at runtime."
2. (p. 9, Sec. 5.3) "Secondly, in its current implementation, the system does not indicate to Unreal that the Blueprint needs to be recompiled when switching alternatives."
3. (p. 9, Sec. 5.3) "P3 stated that they preferred SuBViS because they were able to employ trial and error safely, since they were not afraid to lose their previous work."
4. (p. 7, Sec. 4.1) "We recruited four unpaid participants (all males) from relevant technical undergraduate and graduate programs."

## Limitations and how strong the evidence is
- Very weak for any quantitative claim: four participants, no statistics, authors' own tool, Likert self-ratings. The authors say the conclusions that can be drawn confidently are limited (p. 10).
- The participants were new to Unreal, and the authors say the study "suffered from the participants' inexperience" (p. 10).
- Qualitative findings (users like safe branching; the UI is easy to learn) are plausible but come from four people.
- The use case is hypothetical, written by the authors.
- The "not live" problem is a limit of this implementation (an engine-source modification of Blueprint), not a proof that live alternatives are impossible.

## Relevance to the Gaia agent design
- **Alternatives for design exploration.** Agent-driven design is a try-compare-keep loop. SuBViS shows human users value keeping earlier versions while trying new ones ("not afraid to lose previous work"). In Gaia terms this is `gaia_checkpoint` (designed, not built) and snapshots, not just a linear undo stack. The design's "named best-so-far checkpoint" fits this.
- **Non-live alternatives are a real cost.** SuBViS needed a manual recompile and could not change a running game, which made participants confused. For the agent, every "switch to alternative B" must include the steps that make it take effect (reload code, or restore a snapshot) and must verify the change is visible (inspect, screenshot). Do not let the agent assume a switch has taken effect.
- **No state transfer.** The paper does not say how state would carry over from one alternative to another (the review's phrase "no runtime state transfer" is an inference; see below). B9 covers state migration.
- **Side-by-side comparison** was the users' most obvious wish. For the agent, a contact sheet of frames from each alternative (`gaia_frames`) is the cheap version; parallel game instances are the expensive version and are untested here.
- Not covered: MCP safety, latency numbers.

## Corrections to the current literature-review.md entry (B8)
Entry: "a Blueprint tool with node-level and graph-level 'alternatives'. Switching alternatives did not change behaviour while the game was running and required a manual recompile. No runtime state transfer."
- **Correct:** node-level and graph-level alternatives; switching did not change behaviour in a running game (p. 9); a manual recompile was needed (p. 9).
- **Imprecise:** "No runtime state transfer." The paper does not discuss state transfer. It says behaviours cannot be switched at run time. The review's wording reads as if the paper analysed state transfer. Rewrite as "no runtime switching" or "no live switching".
- **Add:** study size and type: a four-participant, within-subject, think-aloud pilot; ratings "equal or higher" with no statistics (p. 8). The "manual recompile" was a gap in the implementation (the system did not mark the Blueprint dirty), not a design rule.
- **Add:** the positive qualitative finding (trial and error without fear of loss) that supports checkpoints.
- Metadata (FDG 2019, 11 pages, DOI 10.1145/3337722.3337740) is correct. The review's "1-11" is the page count.
