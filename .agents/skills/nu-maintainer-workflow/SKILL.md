---
name: nu-maintainer-workflow
description: >-
  Research, implement, clean up, and prepare changes that touch the Nu engine or are intended for
  bryanedds/Nu. Use for upstream issue/PR work, maintainer-sensitive refactors, dependency migrations,
  convention mining, and post-merge learning.
---

# Nu maintainer workflow

Use current source to implement behavior and repository history to understand intent. Never treat a commit
message, review comment, secondary report, or old sample as a current API contract.

## 1. Pin and classify evidence

Record the target repository, branch, and commit. Classify each relevant observation as:

- **current fact:** visible in checked-out source, project files, assets, compiler output, or tests;
- **upstream fact:** visible at a named upstream commit;
- **maintainer statement:** issue, review, discussion, or commit explanation;
- **historical rationale:** useful intent from old code or a removed implementation;
- **secondary synthesis:** `nu-chat-analysis`, DeepWiki, or generated reports;
- **inference:** a conclusion supported by the above but not directly stated.

Use `.agents/context/nu-maintainer-evidence.md` for durable observations. Keep task-specific research in the
task or PR, not in repository-wide instructions.

## 2. Reconstruct the change surface

Before editing:

1. Search the symbol, property, event, asset, package, and project reference across the repository.
2. Inspect the implementation, callers, tests, sample games, Gaia template/default assets, and source order.
3. Read the issue or PR that introduced the behavior when rationale matters.
4. Compare proposed code with the final merge and any immediate maintainer cleanup.
5. Identify player-visible and editor-visible behavior that compilation cannot cover.

Do not infer an API from a snippet. Dependency APIs and Nu wrappers change; inspect exact signatures and
package source when migrating them.

## 3. Shape the smallest coherent diff

A reviewable Nu change usually contains one behavioral claim plus the necessary implementation, tests,
samples, project metadata, and assets. It should not accumulate opportunistic renames or formatting.

Apply cleanup deliberately:

- use Nu's existing helpers and direct supported APIs;
- put repeated magic values in the closest existing constants module;
- remove obsolete compatibility aliases, redundant references, duplicate properties, dead code, and
  forwarding bindings used only once;
- retain interesting intermediate bindings for debuggability and semantic names;
- keep namespace/open/module blocks, stepped indentation, Lisp-style bracing, tuple spacing, source order,
  and EOF convention consistent with adjacent Nu code;
- use `UpperCamelCase` for true constants and intent-revealing names for lifecycle booleans and operations;
- preserve comments that explain contracts, units, frames, winding, ownership, loader behavior, or why a
  lower-level path is necessary; remove comments that only narrate syntax.

Inlining and extraction are not goals by themselves. Prefer the form that exposes the domain operation,
supports debugging, and avoids a redundant public or module-level name.

## 4. Preserve supported behavior

Implement through World, dispatchers, facets, entities, events, and asset tags when they own the behavior.
Use a backend directly only for a backend-specific capability or when Nu has no supported path, and document
that reason next to the test or integration.

Package migrations should normally land on the current direct API rather than create a compatibility module
for removed names. Preserve units, geometry, ordering, initialization, optional-event semantics, and
resource lifetime before tuning behavior.

## 5. Validate by dependency radius

At minimum:

- build the directly changed project with the intended target framework;
- run focused tests for the claim;
- run Nu integration tests when the supported path is Nu;
- build and manually exercise every sample or game named by the affected integration;
- load in Gaia and test code reload for dispatcher, facet, property, serialized-scene, or default-asset
  changes;
- propagate default assets and validate runtime loading when shaders/assets changed.

Do not collapse partial outcomes. Report assertion counts, process exit, native teardown failures, warnings,
manual controls, runtime logs, and untested platforms separately.

## 6. Prepare the upstream explanation

A useful issue or PR states:

- the observable bug or invariant, not merely the edited code;
- exact current-source evidence and dependency version/commit where relevant;
- why the chosen layer owns the fix;
- automated commands and results;
- manual samples and interactions exercised;
- known failures, environmental limits, and intentionally deferred work.

Keep the diff small enough that the maintainer can merge or reproduce it independently. If the maintainer
manually merges with cleanup, inspect that commit afterward and update durable guidance only when the lesson
recurs or is explicitly stated.

## 7. Final cleanup pass

Review whitespace and comments as semantic material, not cosmetics. Check for:

- accidental terminal newlines in F# or Markdown, or missing newlines in C-style files;
- a generic name replacing a domain name;
- a duplicated constant or default;
- a new dependency edge that is not required;
- source files in the wrong F# compile order;
- comments whose subject no longer matches the code;
- a test that proves a backend but not the Nu path;
- a claimed pass whose process actually failed;
- unrelated changes that should be split.