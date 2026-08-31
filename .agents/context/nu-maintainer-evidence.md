# Nu maintainer evidence notes

## Purpose

This file records source-backed observations that are useful across multiple tasks in this repository. It is
not a substitute for checking the current Nu source, and it does not claim to exhaust Nu's history.

**Last researched:** 2026-09-01  
**Upstream observed:** `bryanedds/Nu` `master` at `e0ad340faf107244676d96fd0ea01b5b8b8a632c`  
**Secondary corpus observed:** `Happypig375/nu-chat-analysis` `main` at
`59f2bdee94575a087eda6b5278dcafdd68084a46`

Historical and recent searches covered commits whose messages include cleanup/clean-up, recent maintainer
commits, selected issue/PR conversations, final merged code, and the analysis repository. The examples below
are representative high-signal evidence, not an assertion that every cleanup commit was manually audited.

## Evidence discipline

Use these classes:

1. **Checked-out fact:** target branch source, project files, assets, compiler, tests, or runtime.
2. **Pinned upstream fact:** visible at the recorded upstream commit.
3. **Maintainer statement:** issue, PR review, discussion, or commit text by Bryan Edds.
4. **Historical rationale:** old or removed code that explains intent but may no longer be correct.
5. **Secondary synthesis:** `nu-chat-analysis`, DeepWiki, or generated summaries.
6. **Inference:** a conclusion supported by evidence but not directly stated.

Only the first two classes establish a current API. Maintainer statements establish preference or rationale
at their date. Secondary synthesis is useful for discovery and cross-linking, never as a source-free oracle.

## Durable observations

### Source and behavior before abstraction

Current source and final merged behavior outrank proposal text. Nu reviews commonly ask for preservation of
the sample/game behavior reached by an engine change, not merely a compiling backend. For the Box2D.NET
migration in PR #1437, the maintainer explicitly required manual checks of Sand Box 2D, Blaze Vector, and
Jump Box, and asked that a post-assertion Vulkan teardown crash be reported as a potential bug rather than
described as an unqualified pass.

**Agent consequence:** define the observable integration path, run focused automation, manually exercise
affected samples, and report assertion success separately from process failure.

### Small, direct, coherent changes

Maintainer merges often include cleanup and may manually extract a small fix from a larger or conflicted PR.
Compatibility aliases, redundant project references, repeated default properties, dead code, and simple
forwarding bindings tend to be removed when the direct supported API is clear. This does not license broad
drive-by formatting.

**Agent consequence:** make one reviewable behavioral claim with the implementation, tests, samples,
metadata, and assets it actually requires. Split unrelated semantics.

### Constants and domain names

In PR #1437, a duplicated Box2D default was rejected as a magic value and moved to a shared Nu constant.
Recent cleanup also changed true module constants to `UpperCamelCase` and replaced generic lifecycle names
with intent-revealing ones.

**Agent consequence:** share repeated physical/protocol values in the closest existing constants module;
name lifecycle and domain operations for what they do.

### Context-sensitive simplification

Recent cleanup removes one-use message/anchor aliases and redundant references, while `Standard.md`
explicitly values local bindings for interesting intermediate results and debugging.

**Agent consequence:** remove names that only forward syntax; retain names that expose a concept, unit,
invariant, reused value, or useful breakpoint. Inlining is not a line-count objective.

### Comments are contracts

Maintainer edits repeatedly narrow comments to the exact contract: frame-finalization timing, 2D-only backend
support, loader expectations, or why a direct backend test is necessary. Stale TODOs and narration are
removed. Ordinary inline comments are often compact/lowercase in local style, while public documentation
comments remain complete descriptions.

**Agent consequence:** preserve why, units, ownership, lifecycle, frames, winding, and non-obvious exclusions.
Delete commentary that merely restates syntax or no longer matches the code.

### F# formatting and file shape

The recurrent style is compact namespace/open/module spacing, stepped indentation, Lisp-style bracing,
intentional-case-first matches, Nu vector helpers, deliberate F# source order, and no terminal newline for
F# or Markdown. Bryan's PR #1435 comment states that terminal newlines are added only to C-style files.

**Agent consequence:** follow `Standard.md` and adjacent code, including project-file/source-order and EOF
conventions; do not normalize Nu to a generic formatter's style.

### Test at the owning layer

A direct backend test is appropriate when a feature is intentionally available only in that backend; the
merged particle-physics work documents that exception. Otherwise a hand-built backend world does not prove
Nu's plugin/World/entity integration.

**Agent consequence:** test through Nu by default. Use direct backend tests only for a backend-owned claim and
state why the World path cannot cover it.

### Optional output must not suppress core state

Merged particle-event handling continued processing required removal/state information when message
publication was disabled, while avoiding optional integration payload allocation.

**Agent consequence:** test enabled and disabled paths. Optional events, rendering, or diagnostics may reduce
work, but must not skip state transitions required for correctness.

## Representative primary evidence

- `e0ad340faf107244676d96fd0ea01b5b8b8a632c` — comment and naming cleanup around frame finalization.
- `ea0f8cda127e8d8b3902c665bde1c18c346fc9fd` — concise comments, removal of one-use binding, clearer
  conditional formatting.
- `6f936b975928ae0cb8283fc0a0ce9b1430ceb26f` — constant casing, compact file structure, Nu helpers,
  redundant reference removal, F# / project-file EOF.
- `cec288d2d0bc306570d6df08fa52451a2172754e` — PR cleanup, direct-backend test rationale, Nu vector helpers,
  source order, and formatting.
- `0e878a32abbfe7ad72d3b9a5e3c68687a05c072c` — manual merge of particle-physics work with integration,
  properties, events, and tests.
- Nu PR #1435 issue comment `5470438220` — explicit C-style-only terminal-newline convention.
- Nu PR #1437 review `5061787900` and approval `5062845375` — manual sample preservation and accurate
  reporting of teardown failure.
- Nu PR #1437 review comment `3890412091` — shared constant instead of duplicated magic value.
- Nu PR #1410 conversation — verify the exact subject/call site; maintainers may extract an isolated fix and
  defer review of the larger change.

Older cleanup history also corroborates Lisp-style bracing, intentional case order, comment/TODO cleanup,
redundant binding/open removal, and naming normalization. Old commits remain rationale, not current API
evidence.

## Secondary evidence use

`Happypig375/nu-chat-analysis` contributes a useful research discipline:

- preserve source attribution;
- separate raw/source-derived evidence from synthesis;
- abstain when provenance is absent;
- keep deterministic derived artifacts rebuildable;
- distinguish source facts, sparse evidence, synthesis, and inference.

Its reports about governance, review friction, trusted contribution, or adoption are hypotheses to verify
against primary Nu evidence before encoding as project rules.

## Volatile facts to re-check

Always re-check these rather than copying this file:

- physics backend package names and versions;
- target frameworks and SDK commands;
- World constructor signatures and dependency records;
- ImSim operator edge semantics;
- event/property helper names;
- native loader paths and platform workarounds;
- Gaia template/default-asset paths;
- serialized property and dispatcher names;
- sample projects affected by an engine subsystem.

## Update protocol

Update this note only when evidence is durable across tasks or an explicit maintainer statement resolves a
recurring ambiguity. Add the source identifier, date/commit, what was observed, and whether it is fact,
statement, history, synthesis, or inference. Remove or qualify observations invalidated by current source.