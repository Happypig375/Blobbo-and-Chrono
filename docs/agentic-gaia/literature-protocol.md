# Literature protocol extraction (from research-workflow-evaluation; read-only)

Sources read: AGENTS.md, CLAUDE.md (`@AGENTS.md`), README.md (skimmed only), docs/protocol.md sections 1-8 and recovery record, docs/source-credibility.md. Not read: method-development.md, zotero-acquisition.md, research-agent-workflow.md. Nothing was modified or executed.

## Rules that apply to us

**Read depth** (protocol section 5, recovery record). Use exactly one of: `metadata | abstract | excerpts | selected_body | indexed_body_complete | original_verified`. Keep these states distinct: tool-fetched, model-visible, read, verified.
- `source: abstract` with `hasMore:false` means only the abstract was exhausted.
- Repeated Scite search excerpts, Consensus passage flags and cached PDFs are NOT a complete paper read.
- For fulltext, advance offset by `returnedChars` until done, and record whether the whole indexed body was completed.
- Extraction can scramble tables and figures; check the original when a claim depends on them.
- A Zotero note by another agent is a pointer, not our reading.

**Provenance per work.** Record DOI or other ID, related versions (preprint/final/co-publication), URL or hash, section/page/table, actual read depth, missing sections, study design, comparator, outcome, supported finding, limitations and contradictions.
- Keep source finding, user report, assistant interpretation and proposal separate.
- DOI verification proves identity, not that the source supports a sentence.
- Do not count each DOI as an independent study.

**Discovery record.** For each query log the tool, query, filters, offsets completed, actual counts, next offset, and branch state (`pending | exhausted | repaired | deprioritized | blocked`).
- Scite: use `limit:20` and paginate while a branch is useful (advance by returned count).
- Consensus: native `page_size:100`.
- Two services returning the same paper is not independent corroboration.

**Screening.** Mark each work include / uncertain / exclude with a criterion-based reason. An inaccessible body is "uncertain", not "exclude". Keep an access manifest for closed papers (DOI/title, relevance, routes tried, missing sections). Do not bypass access controls or buy articles. Open-access routes (arXiv, ACL Anthology) are fine.

**Credibility gate** (source-credibility.md). Before a source supports a manuscript claim, check the exact version and venue.
- Use one of the labels `eligible_for_claim_subject_to_article_appraisal | context_or_prior_art_only | hold_from_substantive_support | not_yet_assessed`.
- Unverified preprints and venues default to context/prior-art only.
- Retrieval does not certify peer review.

**Stops.** Branch stop, answer stop, systematic-search stop and study-start decision are different. Budget exhaustion, tool failure and incomplete access are operational stops, not saturation. Keep the unexamined frontier; do not invent a recall estimate.

**Evidence handling.**
- Judge evidence against the claim's scope: benchmark-bounded results do not transfer to deployment.
- A full-system win does not isolate one component's effect.
- Distinguish insufficient evidence from refutation.
- Keep compact records; raw provider JSON is disposable.

**Zotero.** Read-only for agents (also a machine rule). Never write to Zotero, WebDAV or the database.

**Maturity gate.** Do not start a new experiment while decisive prior work is unread. Compare purpose, mechanism, evaluation and application. If critical access is blocked, keep a bounded conclusion and state the missing evidence.

## Citation format
The protocol sets no bibliographic style. It requires normalised DOIs or other legitimate IDs, plus version/date precision (preserve dates, no plausible reconstructions). We therefore use APA-like entries with `https://doi.org/<doi>` or the arXiv/ACL URL, the arXiv version if known, and the venue labelled "per arXiv comment / unverified".

## Deviations in this run (to log honestly)
- Scite `limit` was 8-15, not 20, with no pagination beyond page one.
- Consensus was unavailable (monthly quota exhausted; see notes).
- Supplementary open-access reads used WebFetch. Its summariser is a small model, so those entries count as "abstract (paraphrase via WebFetch)", not verbatim.
- No benchmark runs, no litfetch, no frozen-material access.
