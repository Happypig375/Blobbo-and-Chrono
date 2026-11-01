# B2. Liveness in the Age of Agents: Are We Back to Compile-and-Run Cycles?: reading note (abstract only; full text not retrieved)

## Citation
Mattis, T., Taeumel, M., Böhme, L., & Hirschfeld, R. (2026). *Liveness in the Age of Agents: Are We Back to Compile-and-Run Cycles?* In *Companion Proceedings of the 10th International Conference on the Art, Science, and Engineering of Programming* (‹Programming› 2026, Munich, 16-20 March 2026), PX/26 (12th Programming Experience Workshop), pp. 71-77. ACM. https://doi.org/10.1145/3801119.3801131 . Affiliations: Hasso Plattner Institute, University of Potsdam. Conference page: https://2026.programming-conference.org/details/px-2026-papers/12/Liveness-in-the-Age-of-Agents-Are-we-Back-to-Compile-and-Run-Cycles- (talk Mon 16 Mar 2026, 16:00-16:30, Seminar Room 123).
Venue claim verified: Crossref record for 10.1145/3801119.3801131 (title, container "Companion Proceedings of the 10th International Conference on the Art, Science, and Engineering of Programming", pages 71-77, 12 references, printed date 2026-03-16, online 2026-04-12); the Conference Publishing table of contents (`https://www.conference-publishing.com/toc/PROG26PX`) lists it as article `prog26px-id12-p`, type "Full Paper".

## Versions
- Single published version (ACM companion proceedings, 2026). No arXiv, Zenodo or author-hosted preprint found (arXiv API title/author searches returned only unrelated papers by the same group).
- Version read: **none of the paper body**. Only the abstract as published on the conference page was read (verbatim, via curl), plus Crossref and OpenAlex metadata.
- Check date: 2026-11-01.
- Reading note in `~/workspace/research-workflow-evaluation`: none (grep for the title and authors found nothing).

## Access and read depth
**Access state: failed (not closed).** OpenAlex lists the work as hybrid open access with licence `cc-by` and the published version at the ACM Digital Library (landing page only, no PDF URL recorded). Attempts and outcomes:
- `https://dl.acm.org/doi/pdf/10.1145/3801119.3801131` (curl) and `https://dl.acm.org/doi/10.1145/3801119.3801131` (WebFetch): HTTP 403, Cloudflare challenge (`cf-mitigated: challenge`). Not retried or worked around (bot-check; the machine rules say not to evade).
- `https://www.conference-publishing.com/Proc/PROG26/prog26/prog26px-id12-p.pdf` (the "Link to Publication" on the conference page): HTTP 404; the same 404 for `prog26px-id1, -id5 ... -id11-p.pdf`, so the pattern is dead for this proceedings.
- arXiv (export API: title and author queries): no copy. A web search found only the conference page. Zenodo: the PX table of contents lists one Zenodo DOI (10.5281/zenodo.18889419) that I could not tie to this paper (not opened).
- Author pages and Semantic Scholar not tried (search results gave none).
Read depth label: `abstract` (verbatim abstract from the conference page; not a WebFetch paraphrase). **Study design, method, examples, figures, the 7 pages of argument and the 12 references are unread.** Needs a human/`litfetch` route: hand the DOI (10.1145/3801119.3801131) to the orchestrator or user, noting that OpenAlex records a CC BY licence (not checked on the ACM page itself).

## What the paper is (abstract only)
Per the abstract ("to discuss the impact agents have on the overall programming experience"), a discussion-style paper for the Programming Experience (PX) workshop. It applies Tanimoto's liveness levels 5 and 6 together with Norman's gulfs of execution and evaluation to ask what agent-assisted programming does to feedback-loop length and programmer experience. Whether it contains any empirical component (observation, user study, case study) cannot be determined from the abstract.

## Key results (what the abstract states; locator: conference-page abstract)
1. Premise: experienced programmers in a live programming environment produce quality software in reasonable time, and novices also do well because live tools "foster short feedback loops" that link thoughts to executable code and observable behaviour.
2. With agents, the programmer states a goal in natural language and the agent makes larger coherent edits; but "agents will take their time to think", and programmers still need skills to judge generated results. The authors question how live and how short that loop feels.
3. Using liveness levels 5 and 6 with Norman's gulfs, they report that agent support is "far from predictive": prompts take noticeable effort to clarify, or to derive, the programmer's current goals.
4. They conclude that the interaction cycles "might feel quite productive at times", but the experienced Gulf of Evaluation can be too much for novices if the results are too sophisticated.
5. Outlook: tailor the programming experience around agents to improve both prompting and reviewing, so agents do not return programmers to "lengthy compile-and-run cycles".

## Verbatim quotes (all from the conference-page abstract; no page locators exist for the unread PDF)
1. "A novice can also perform well because live programming tools foster short feedback loops"
2. "However, agents will take their time to think, and programmers require skills to make informed decisions about an agent’s generated results. How “live” is that? How “short” does this feedback loop feel?"
3. "We realize that the agent support is far from predictive as prompts take noticeable effort to clarify (or derive) a programmer’s current goals."
4. "We also conclude that while the interaction cycles might feel quite productive at times, the experienced “Gulf of Evaluation” can be too much for novices if the results are too sophisticated."
5. "does not take us back to the times where lengthy compile-and-run cycles disrupted our thoughts, flow, and motivation."

## Corrections to the current literature-review.md entry (B2)
Entry: "Mattis ... (2026). PX/26 workshop at ‹Programming› 2026 (talk abstract). [conference URL]" / "Route: WebSearch. Depth: abstract (WF). Workshop abstract only; no study design visible." / "uses Tanimoto's liveness levels and Norman's gulfs. Argues agents can bring back slow compile-and-run cycles and that the gulf of evaluation may overwhelm novices. Position-level evidence." / "Informs: why a REPL/world-inspection agent should keep feedback latency low and reviewable."
- **"talk abstract" is wrong as a bibliographic type.** It is a seven-page paper (pp. 71-77, 12 references; listed as "Full Paper") in the ACM companion proceedings, DOI 10.1145/3801119.3801131, CC BY per OpenAlex. The review should cite the DOI and the proceedings, not only the talk page. The review's "no study design visible" is correct only as a statement about the abstract; the paper itself is unread.
- **Abstract nuance the entry omits:** liveness levels 5 and 6 specifically; the claim that agent support is "far from predictive" (the agent must have the programmer's goals clarified or derived); and that the evaluation gulf is stated as a cost for novices when results are "too sophisticated", i.e. it concerns reviewability of large agent edits, not only latency. The abstract does not give any latency measurement.
- **Direction of the argument:** the paper is about a human programmer working with an agent. It does not (per the abstract) address an agent acting as the live programmer in a game editor. The review's "Informs" line (keep agent feedback latency low and reviewable) is an inference from the abstract's framing, not a finding.
- Route: the review says "WebSearch" and "abstract (WF)"; this note's abstract is from the conference page fetched directly with curl (verbatim). Access state is "failed" (ACM DL Cloudflare 403; Conference Publishing PDF 404), not "closed" or "unknown".
- Title casing differs between sources: the conference page uses "Are we Back to Compile-and-Run Cycles?", ACM/Crossref "Are We Back to Compile-and-Run Cycles?".

## Design relevance (limited by the depth read)
- Frames the design question rather than answering it: the experienced length of the feedback loop (agent "thinking" time) and the reviewability of larger agent edits are the human-experience costs of delegating to an agent. For Gaia this supports keeping each eval/step result small and quickly checkable (a diff, a screenshot, a named assertion result) and keeping edits reviewable by the human who is watching the live editor.
- "Far from predictive" suggests the agent should confirm or state its inferred goal before large edits; a cheap probe/plan step is consistent with this but is not tested in the abstract.
- Evidence level appears to be position/discussion (abstract wording; body unread); use it only as context for the liveness framing (Tanimoto levels 5-6, Norman gulfs), not for quantitative latency thresholds.

## Limits / credibility
- Peer-review status of PX/26 companion papers: not verified (PX is a workshop; the proceedings list it as a "Full Paper"). Credibility-gate label: `hold_from_substantive_support` until the body is read; usable as `context_or_prior_art_only` for the framing only.
- Hirschfeld is also a coauthor of B3 in the review (live game development environment); this paper's own evidence base is unknown.
- All four authors are at Hasso Plattner Institute / University of Potsdam; no conflict statement could be checked.

---

## Full-text addendum (Opus, 2026-11-01 ~17:50 HKT): supersedes "abstract only" above
- **Access:** retrieved with `litfetch` through the campus browser after a human cleared the ACM bot check
  (status `entitled`; report `docs/agentic-gaia/litfetch-b2.json`). The PDF is the published version, CC BY 4.0
  (licence line on p. 71). It is not committed; it lives at `~/papers/inbox/10.1145_3801119.3801131.pdf`.
- **Read depth:** full text, all 7 pp. (71–77), including Figs. 1–5 and the 12 references.
- **Type:** a position/discussion paper. There is no empirical study, measurement or user study. Its evidence is
  argument plus cited work (e.g. Ferdowsi et al. CHI'24 [3], Vaithilingam et al. CHI'22 [12], Subramonyam et al.
  CHI'24 [9]).

### Key arguments (with locators)
1. **Two opposing forces** (§3.1, p. 73). Agents raise the scope of change to "strategically predictive" (level 6),
   while "the speed and continuity of feedback has dropped back to compile-and-run levels, but instead of compile
   time we are confronted with generation time". The runtime can stay live even when the experience does not: "a
   live programming environment operated through an agent remains live, but the programmer's experience does not"
   (p. 73).
2. **Norman's gulfs** (§4.1, p. 74).
   - Agents *collapse* the gulf of execution, and actions are performed "through agent tools provided by the
     environment, such as protocols like MCP".
   - They *widen* the gulf of evaluation, because large changes are hard to perceive. The notation becomes part of
     what must be evaluated.
   - They add Subramonyam et al.'s **gulf of envisioning** (forming an unambiguous intent).
   - A worked jump-and-run example (gravity vs jump height; p. 74) illustrates the seven stages. It is the same
     domain as our Breakout and gameplay-tuning use case.
3. **Feedback has three parts** (Fig. 4, p. 75): what the agent did and why; its effect on the code; and the runtime
   consequences. "Liveness can help communicate all three perspectives."
4. **Proposals** (§5, pp. 75–76), none of them evaluated:
   - Clarify intent "live", grounded in runtime objects ("drag-and-drop" objects into prompts; agents refer to
     objects; offer several interpretations, in the spirit of level 5; Fig. 5).
   - Show agent changes in a running system through well-scoped, representative examples, including invariants
     ("example scenarios that intentionally did not change their outcome"). They call this a "frame of
     explanation" and list it as open work.
   - The agent explains itself (context used, feedback received), ideally as a first-class, manipulable artifact.
   - Use generation time well: pair-programming-style continuous, demonstrated explanation.
   - **Expose live tools to agents.** "Since live programming enables humans to gather feedback more efficiently,
     it will likely also benefit agents. This opens up the design space for exposing live and run-time tools to
     agents, ranging from smaller, low-level tools like probes and inspectors to high-level tools such as
     profilers, call graph analyses, or omniscient debuggers." (p. 76)
5. **Conclusion** (§6, p. 76): the running program inside a live environment should become "the shared artifact
   about which programmers and agents converse".

### Verbatim quotes, checked against the PDF text
1. "a live programming environment operated through an agent remains live, but the programmer's experience does not." (p. 73)
2. "Agents widen the Gulf of Evaluation." (p. 74, run-in heading)
3. "Liveness can help communicate all three perspectives of feedback." (p. 75, Fig. 4 caption)
4. "This opens up the design space for exposing live and run-time tools to agents, ranging from smaller, low-level tools like probes and inspectors to high-level tools such as profilers, call graph analyses, or omniscient debuggers." (p. 76)
5. "we expect the running program inside a live programming environment to become the shared artifact about which programmers and agents converse" (p. 76)

### Corrections to the abstract-only note and the review entry
- The abstract-level reading was accurate, but the paper says more. Beyond human-experience costs, it explicitly
  argues for the agent-side direction this spike takes: exposing probes and inspectors to agents and making the
  running program the shared artifact. It is position-level support, not evidence.
- "Informs: keep feedback latency low and reviewable" stands. Add three points:
  - evaluation needs all three feedback parts (Fig. 4);
  - invariant examples (what did *not* change) belong in the evidence;
  - the human watching the live editor needs the agent's changes demonstrated, not just described.

### Design relevance for Gaia
- Our tools already return two of the three feedback parts: eval/diagnostics (code effect) and
  screenshots/frames/inspect (runtime). The third, "what the agent did and why", is the transcript plus Gaia's
  Interactive window. `gaia_diff` (next) and the end-of-session contact sheet address the widened gulf of
  evaluation.
- `gaia_assert` with *unchanged-outcome* scenarios matches their "frame of explanation" including invariants.
- Live, object-grounded intent is a natural extension: selected entity → prompt context. The bridge binds
  `SelectedEntity` already.

### Leads (not read)
- Ferdowsi et al., CHI'24, "Validating AI-Generated Code with Live Programming", doi 10.1145/3613904.3642495. This
  is an empirical study and the most relevant follow-up.
- Thiede et al., Onward!'24, "Talking to Objects in Natural Language", doi 10.1145/3689492.3690049.
