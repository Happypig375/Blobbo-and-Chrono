# D2. Vision-Guided Iterative Refinement for Frontend Code Generation: reading note

## Citation
Sansford, H., Law, D. H. C., Liu, W., Tripathi, A., Agarwal, N., & van den Burg, G. J. J. (2026). *Vision-Guided Iterative Refinement for Frontend Code Generation.* arXiv:2604.05839 [cs.AI]. https://doi.org/10.48550/arxiv.2604.05839 (arXiv abs: https://arxiv.org/abs/2604.05839).
Venue claim (arXiv comment and PDF header): "Accepted at ICLR 2026 Workshop: AI with Recursive Self-Improvement". A workshop acceptance, not a main-track paper. Affiliations: University of Bristol (first author, "work done while at Amazon") and Amazon AGI.

## Versions
- Latest version: v1 only (submitted 7 Apr 2026, 13:06:48 UTC; no later revision on the abs page checked 2026-11-01).
- Version read: v1 (PDF `arxiv.org/pdf/2604.05839`, 19 pp.).
- Check date: 2026-11-01.
- Reading note in `~/workspace/research-workflow-evaluation`: none (grep for the arXiv ID and for title keywords found nothing).

## Access and read depth
Open access (arXiv). Read depth: full text, all 19 pp. including the references, Appendix A (Table 4) and Appendix B (all prompt templates). Figure 1 (a chart) was read only through its caption and the numbers in Tables 2-3; the plot image itself was not viewed. Table values were read from `pdftotext -layout` output (cleanly aligned here). Protocol label: `indexed_body_complete` for the text; figures not inspected.

## What the paper is
A "critic-in-the-loop" (CITL) pipeline for natural-language-prompted single-page HTML/Tailwind generation (p. 3-4, Algorithm 1). Generator G (LLM) writes HTML. A renderer R (Selenium) screenshots it. A visual critic V (VLM, fixed to Claude 4.5 Sonnet in every experiment, p. 5) critiques the screenshot. A code critic C (LLM, same model as G) merges the visual critique with the code into a consolidated critique. An improver I rewrites the code. An evaluator E (VLM-as-judge) scores each candidate and the loop retains the best-scoring one (`s_{t+1} >= s_best`). Budget T = 3 cycles in the reported tables. Then LoRA fine-tuning of Distill-Qwen-14B on the CITL-best outputs tests whether the gain can be internalised.

## Key results (with locators)
Setup (Sec. 4.1, p. 5): WebDev Arena real user queries, filtered to 4.4k, 2,000 sampled and split 1,200 train / 200 validation / 600 test. Generators: DeepSeek-R1-Distill-Qwen-14B, Claude 4.5 Haiku, Claude 4.5 Sonnet. Visual critic always Claude 4.5 Sonnet, so the Qwen-14B and Haiku generators are criticised by a stronger model, and only the Sonnet row is true self-improvement.

1. **Judge validation (Sec. 4.2, Table 1, p. 6).** 200 WebDev Arena pairs with a clear human preference (ties excluded). The multi-dimensional judge (separate visual-only and code-only scoring, four 1-10 dimensions averaged) agrees with humans in 69.5% of pairs, 8.5% draws, 22.0% disagreements. The single-score judge: 48.5% agree, 28.5% draws, 23.0% disagree. The "43.3% higher agreement" is relative (69.5/48.5). The disagreement rate is almost the same (22.0 vs 23.0), so most of the gain is fewer ties. Authors cite Li et al. (2025b) for 69.77% agreement with a different judge.
2. **Main CITL results (Table 2, p. 7; mean judge score, 1-10 scale; gain relative to initial; tokens per task).**
   - Distill-Qwen-14B: initial 5.236; cycle 1 5.558 (+6.2%, 15,067 tok); cycle 2 5.642 (+7.8%, 29,853); cycle 3 5.690 (+8.7%, 45,808); **"CITL: Best" 6.165 (+17.8%, 23,662)**; Refine w/o Critic 5.317 (+1.5%, 7,209); LoRA 5.467 (+4.4%, 2,583).
   - Haiku 4.5: initial 7.436; cycles +3.6%, +5.2%, +5.7% (37,604 / 77,787 / 124,599 tok); Best 8.160 (+9.8%).
   - Sonnet 4.5 (self-improvement): initial 7.584; cycles +4.9%, +5.8%, +6.1% (37,642 / 80,472 / 131,550 tok); Best 8.402 (+10.8%).
   - Diminishing returns: cycle 1 delivers about 71% (Qwen), 63% (Haiku) and 80% (Sonnet) of the cycle-3 gain, at about one third of the cycle-3 token cost. Mean score still rises monotonically through cycle 3, with shrinking increments.
   - "Best" is the best of the initial code and cycles 1-3 chosen per task by the evaluator E (Algorithm 1, p. 4), and then scored by the same E. It is therefore a best-of-4 selection under the reporting metric, not the output of cycle 3. The headline 17.8% is this number. Per-cycle means (cycle 3: +8.7% for Qwen) are the fair "what a fixed 3-cycle loop gives" figures. The paper says the best solution is found "after one or more cycles for 86% of tasks" in the Sonnet self-improvement setting (p. 6), i.e. in 14% of tasks the initial code was already the best.
3. **Critic ablation (Sec. 4.3, p. 6).** "Refine w/o Critic" (generator told to improve its own code; no screenshot, no critique) gives +1.5% on Qwen-14B, against +6.2% to +8.7% for CITL cycles. Number of refinement cycles for this baseline is not stated; its token count (7,209) suggests one pass, but the text does not say. This isolates *critique* from *no critique*. It does not isolate *visual* critique from *text-only critique of the same code* (no such arm exists), and the critic is a stronger model than the generator for two of three generators.
4. **Per-dimension breakdown (Table 3, p. 7; Table 4, p. 10).** Qwen-14B: visual task accomplishment +11.2% / +15.0% / +16.8% over cycles 1-3 (best +26.3%); aesthetic +6.9 / +9.4 / +10.4% (best +18.0%); code task accomplishment +7.4 / +8.0 / +9.2%; code quality **falls** -1.7 / -2.8 / -3.2% over cycles (best-of selection +7.6%). For Haiku and Sonnet (Table 4) code quality rises (+4.2/+6.5/+6.9% and +6.5/+6.8/+6.4%) and visual task accomplishment gains are small (+2.5/+3.6/+4.2% and +1.5/+1.8/+2.2%), because their baselines are already high (about 7.8 to 8.1).
5. **LoRA distillation (Sec. 4.3, p. 7).** LoRA (r=16, alpha=32, dropout 0.1, early stopping on validation loss; p. 6) trained on 1,200 CITL-best outputs captures 24.7% of the CITL-Best gain and 50.6% of the cycle-3 gain at single-pass token cost (2,583 vs 2,571 initial). Gains concentrate in aesthetics and code task accomplishment; visual task accomplishment gains are small (+2.9%), "the distilled signal primarily captures general aesthetic and structural improvements rather than fine-grained task-specific visual requirements" (p. 8). Only one model (Qwen-14B) was distilled.
6. **Critique content (Sec. 4.5, Fig. 3, p. 8).** 7,300 critiques classified into 8 categories by Claude 4.5 Sonnet: Visual Polish 42.8%, Missing Elements 25.6%, Implementation 18.9%, Content 5.6%, Interactivity 4.4%, Responsive 1.3%, Components 0.9%, Typography 0.5%. The classifier taxonomy was derived by the same LLM from 50 critiques. Authors conclude a multimodal critic is needed.
7. **Limitations stated (Sec. 5, p. 8).** Reliance on VLM-as-judge (bias, missed accessibility issues); no human-critic ceiling; intermediate signals not distilled; cause of "performance regressions and the diminishing returns of the critique" not understood.

## Verbatim quotes
1. (p. 6, Sec. 4.3) "On the Distill-Qwen-14B model this approach produces only a 1.5% improvement, confirming that critic-guided refinement is the primary driver of performance gains we observe." (sentence is about the "Refine w/o Critic" arm)
2. (p. 6, Sec. 4.3) "Looking at the best solution obtained through the cycles, we find a performance improvement of 17.8% with the Distill-Qwen-14B model as code generator, and 10.8% in the self-improvement setting with Claude 4.5 Sonnet."
3. (p. 4, Sec. 3.1) "Across iterations, the system retains the highest-scoring solution" (the best-of selection that produces the "Best" rows)
4. (p. 7, Sec. 4.4) "By contrast, code quality exhibits a slight decline across CITL cycles"
5. (p. 11, App. B.1, generator prompt) "Make sure ALL content is visible in a single screenshot" (the task is deliberately constrained to a single static screenshot)
6. (p. 8, Sec. 5) "our evaluation relies heavily on a VLM-as-a-Judge, which can exhibit biases or other deficiencies"

## Corrections to the current literature-review.md entry (D2)
Entry text: "a VLM critic on rendered pages guides revisions, with gains up to 17.8% over three cycles on WebDev Arena requests. LoRA distilled about 25% of the loop's gains." and "Informs: render, critique, revise loops; cycle count."
- Correct as far as it goes (17.8% and 24.7% both verified), but missing the key nuance: 17.8% is the *best-of-{initial, cycle 1-3} per task, selected and scored by the same VLM judge*, for the weakest generator (Distill-Qwen-14B) criticised by a stronger model (Claude 4.5 Sonnet). A fixed three-cycle loop gives +8.7% (Qwen-14B), +5.7% (Haiku 4.5), +6.1% (Sonnet 4.5 self-critique); the 17.8% / 9.8% / 10.8% figures include best-of selection. Suggest wording: "mean judge-score gain +6 to +9% after 3 cycles (up to +17.8% with best-of selection, same judge)".
- "LoRA distilled about 25%": 24.7% of the best-of gain, but 50.6% of the cycle-3 gain; single model, judge-scored.
- Missing for "cycle count": gains concentrate in cycle 1 (63-80% of the cycle-3 gain) with token cost growing roughly linearly in cycles (Haiku: 37.6k, 77.8k, 124.6k tokens vs 6.8k for the initial code). This supports a small cap (1-2 rounds) rather than open-ended looping.
- "A VLM critic on rendered pages" is accurate, but the loop's critique pipeline is two-stage (VLM looks at the screenshot only; a separate LLM merges that with the code). There is no ablation of visual vs text-only critique. The evidence that *vision* specifically is needed is circumstantial (Fig. 3 category shares), not an experiment.
- Venue: a workshop paper (ICLR 2026 Workshop on AI with Recursive Self-Improvement), not stated in the review entry. Authors are Amazon AGI plus one academic.
- Scope: single static page, one screenshot, no interaction, no time dimension. The review's use of D2 for runtime/frame feedback would overreach.

## Design relevance
- **Iteration cap.** Most of the gain arrives in the first critique round (cycle 1 = +6.2/+3.6/+4.9% of the +8.7/+5.7/+6.1% at cycle 3); cumulative token cost grows roughly linearly (Haiku 37.6k tokens after cycle 1, 124.6k after cycle 3, vs 6.8k for the initial code). For Gaia, default to 1-2 look-critique-edit rounds per request and expose the cap to the user. Cite as web-UI evidence only.
- **Keep the best-so-far.** The pipeline retains the best-scoring candidate across cycles, and in the Sonnet self-improvement setting 14% of tasks never beat the initial code. This supports the snapshot/undo design: a later edit can be worse, so the agent should be able to return to a previous state. The paper's selection used a judge score. In Gaia the analogue needs a state assertion or a human choice, because a model-as-judge selecting and scoring is optimistic (see Limits).
- **Separation of looking and editing.** The screenshot goes to a separate visual critic whose output is text; the code-editing model never sees pixels (p. 4). This is a design option for the screenshot/contact-sheet tool: a descriptive pass over frames, then an edit pass over structured findings. No evidence in this paper that it beats giving the screenshot directly to the editing model.
- **Un-grounded self-refinement is weak.** "Refine w/o Critic" +1.5% vs +6-9% with critique supports the review's "end each step with an external observation". The ablation is not clean, since the critic is also a stronger model.
- **Only static visual quality.** Prompts forced all content into one screenshot (avoid modals/popups). Nothing here informs motion, input-driven behaviour or multi-frame evidence.

## Limits / credibility
- Workshop preprint, v1 only; credibility-gate label: `context_or_prior_art_only`.
- Evaluation is entirely VLM-as-judge (model not named in the text I read), validated by 69.5% pairwise agreement on 200 pairs with human arena votes. The selection step uses the same judge as the reporting metric (optimistic "Best" rows). No human study of the outputs, no executable or DOM-based check.
- Critic is stronger than the generator in 2 of 3 settings (confound with visual modality). No text-only-critic arm.
- Judge scores are mean 1-10 ratings; "gain %" is relative to the baseline mean, so +17.8% on 5.236 is +0.93 points; Haiku and Sonnet start at 7.4-7.6 with less headroom.
- Single task family (generic landing pages from arena prompts), single screenshot, Tailwind only; no interactivity.
