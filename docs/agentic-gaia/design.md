# Agentic Gaia: recommended agent interface

Inputs: `investigation.md` (findings 1–4, code references), `literature-review.md` (sources, read depth),
prototype in `prototype/` (verified end to end, 28/28 checks after round-2 hardening and review fixes). Literature references below use the review's ids
(A1, C7 …); most are preprints, so they shape priorities rather than prove effects.

## 1. Recommendation in one paragraph

Expose Gaia through **one small, versioned request protocol on the editor thread** (owner-only local socket,
JSON lines, opt-in), and put **two thin clients** on it: an **MCP server with ~14 task-shaped tools** for agents,
and a **terminal REPL** for humans and shell-only agents. The MCP tools give the agent three *independent*
feedback channels after every change (compiler diagnostics, structured world state, rendered pixels including
short frame sequences) plus the live F# session's own type checker for API lookup. Every mutating call is
wrapped in Gaia's existing world snapshot so it can be undone. Live-world edits are explicitly *temporary*;
designs become *persistent* only through source edits + hot reload or saved scene files. Headless operation on
Linux is Xvfb + Mesa lavapipe today; an in-engine Vulkan readback is the next step for exact, display-free capture.

## 2. Architecture

```
 agent (Claude Code, Codex, …)          human / shell agent
        │ MCP stdio                             │ terminal
 ┌──────▼─────────┐                    ┌────────▼────────┐
 │ gaia_mcp.py    │  tools → ops,      │ gaia_bridge.py  │  F# lines → eval,
 │ (MCP server)   │  screenshots,      │ repl            │  :op {json}
 └──────┬─────────┘  contact sheets    └────────┬────────┘
        └───────────── JSON lines over $XDG_RUNTIME_DIR/nu-gaia-agent.sock (0600) ─┘
                                        │
 Gaia process ──────────────────────────▼───────────────────────────────────────────
  AgentBridge (background threads): accept, parse, enqueue, await reply (timeout_s, default 120 s; withdraw
  on timeout); ping + liveness answered here without the editor thread
  editor thread, once per frame in imGuiProcess → agentProcess:
     drain queue → evalInteractive / tryUndo / setTimeAdvancing / tryReloadCode / inspect / symbol …
     deferred replies keyed by editor frame (advance N, wait N)
  renderer thread → Vulkan (lavapipe on Xvfb, or a GPU) → window → capture
```

Why this split: all FSI and `World` access must stay on the editor thread (`investigation.md` §1), so Gaia's side
is a queue drained in `imGuiProcess`, and the transport stays dumb. A dumb, versioned JSON-lines protocol lets the
CLI and MCP share it ("both" costs ~150 lines), keeps MCP SDK churn out of Gaia's dependency set, and maps
directly onto a named pipe on Windows. An in-process MCP (HTTP) server is a reasonable later consolidation.

## 3. API surface

Tool names as an agent sees them. **Built** = in the spike and covered by `prototype/check_mcp.py`;
**next** = designed, not built.

### Orient
| Tool | Args | Returns | Status |
|---|---|---|---|
| `gaia_status` | – | frame, updateTime, advancing, imperative, project, selection, undoDepth, logSeq | built |
| `gaia_hierarchy` | – | screens → groups → entities (depth-first) with dispatcher names | built |
| `gaia_inspect` | `path` | properties: name, type, symbolic value | built |
| `gaia_symbol` | `name` | F# signature per overload, XML doc, declaration `file:line` (FCS in the live session) | built |
| `nu_docs` | `query`, `limit` | keyword search over `Nu.xml` members and summaries | built |
| `gaia_search_api` | `query`, `kind?` | FCS entity index of Nu, Prime and the project (F# display names + docs + location) | next |

### Act
| Tool | Args | Returns | Status |
|---|---|---|---|
| `gaia_eval` | `code` | `kind` ok/compile_error/runtime_error, FSI echo, warnings, exception, diagnostics with ranges; snapshot first | built |
| `gaia_reload_code` | – | build log (diagnostics), after recompiling the project's sources into the live world | built |
| `gaia_set_property` | `path`, `name`, `value` (symbolic) | new value; snapshot first; uses Gaia's property converters | next |
| `gaia_save` | `what` (group/screen), `file` | writes `.nugroup` etc. so live-world design becomes persistent | next |
| `gaia_input` | key/mouse/gamepad events, frames | injects input for playtesting (SDL event queue on the editor thread) | next |

### Time
| Tool | Args | Returns | Status |
|---|---|---|---|
| `gaia_advance` | `frames` | runs game time for N editor frames, halts, replies after the halt has taken effect; snapshot first | built |
| (`set_advancing`, `wait_frames`) | | protocol ops used by clients | built |

### Observe and verify
| Tool | Args | Returns | Status |
|---|---|---|---|
| `gaia_screenshot` | `view` editor/game, `max_width` | PNG (MCP image content) + saved path | built |
| `gaia_frames` | `count`, `every`, `view` | one contact-sheet PNG of a frame sequence (motion at a fraction of video tokens) | built |
| `gaia_log` | `since` | editor log after a sequence number | built |
| `gaia_diff` | `from` snapshot | changed simulants and properties between two snapshots | next |
| `gaia_assert` | F# predicate, `frames?` | evaluates a boolean over the world each frame for N frames; first failing frame | next |
| `gaia_clip` | `seconds`, `fps` | short WebP/MP4 clip | next |

### Session
| Tool | Args | Returns | Status |
|---|---|---|---|
| `gaia_undo` / `gaia_redo` | – | `undone`/`redone` + status | built |
| `gaia_checkpoint` | `name` / `restore` | named snapshot outside the linear undo stack (best-so-far retention) | next |
| `gaia_ping` | – | liveness answered off the editor thread: last frame, ms since it, running op and duration, queue length, `unresponsive` | built (round 2) |
| `gaia_restart` | `confirm: true` | supervisor restart of the Gaia this server launched (pid checked against `Nu.Gaia.dll`), same project; world/undo/FSI state lost | built (round 2) |
| `gaia_reset_session` | – | restart only FSI (keep the world) after a poisoned but not hung session | next |

Schemas are in `prototype/gaia_mcp.py` (`TOOLS`). Tool descriptions are kept short and actionable, e.g. "Keep
snippets small; a non-terminating eval freezes the editor", following the ACI and tool-description evidence (C7,
E2, E3, E5): a few task-shaped tools, structured errors, no 150-tool catalogue.

## 4. Feedback loop the interface is built for

1. **Orient**: `gaia_status`, `gaia_hierarchy`, and `gaia_symbol` / `nu_docs` before writing code against an
   unfamiliar API. Nu's XML docs are summary-only, so the tool returns `file:line` and the agent reads the source.
2. **Probe**: tiny read-only `gaia_eval`s (types, current values). A compile error comes back with code + line/col
   in ~0.1 s; that is the cheapest possible correction signal.
3. **Act**:
   - *Exploration*: `gaia_eval` creates or changes simulants in the live world. These changes are temporary and
     undoable.
   - *Commitment*: once the result is right, the agent edits the project's `.fs` files and runs `gaia_reload_code`
     (~25 s for Breakout on this server), or saves the scene with `gaia_save` (next).
   - Keeping these two levels explicit is the live-programming lesson (B3, B9): users and agents must know what
     survives a restart.
4. **Observe**: after every act, at least one external signal.
   - `diagnostics` for code.
   - `gaia_inspect` / `gaia_diff` for state.
   - `gaia_screenshot` for layout and colour.
   - `gaia_frames` (or a clip) for anything that moves. Full-text reading confirms that in GameDevBench (A1)
     runtime video, not editor screenshots, carried the gain (Sonnet 4.5 v1: video +14.4 points, screenshot 0.0).
     In D1, screenshots helped mostly by driving exploration that *finds* bugs (+2.85), not as input to the fix
     (+0.65).
   - Replayed input is the strongest runtime check found: in A10, replay before and after an edit caught 76.2% of
     failed edits, against 0% for build-only checks. This argues for `gaia_input` plus `gaia_assert` (next).
   - Self-critique without such a signal is unreliable (C4, C6, C13).
5. **Judge and keep/undo**:
   - Where possible, use state assertions (`gaia_assert`, e.g. "ball stays inside the walls for 300 frames"),
     because "it runs" and "it looks right" both miss logic bugs (A4, A6).
   - On failure, `gaia_undo` and retry.
   - Cap repair rounds at about 3. Gains concentrate in rounds 1–3 (C14, D2, H4–H6). Refine only when a check
     actually failed, because refining passing code breaks it (C15). Return the concrete failure text, not a bare
     flag (H9). After that, roll back and restart the step rather than patching further (H6), keeping a note of
     what failed (F1).
   - Keep a named best-so-far checkpoint (A10).
6. **Report to the human**:
   - Evals are visible in Gaia's Interactive window and Timeline.
   - Screenshots are saved to a private directory.
   - The agent ends with a contact sheet and a summary of what became persistent (B1: designers lose track of
     agent-made changes).

The measured latencies make this loop interactive:

| Operation | Latency |
|---|---|
| eval | 0.1–0.3 s (2.6 s for the first eval, which initialises the session) |
| inspect, hierarchy | ~0.01 s |
| screenshot | ~0.2 s |
| 4-frame contact sheet | 1.5 s |
| symbol lookup | 0.45 s |
| code reload | ~25 s |

This matters because agent latency can bring back slow compile-and-run cycles (B2).

## 5. Safety model (layered)

1. **Opt-in and local only**:
   - The bridge exists only when `NU_GAIA_AGENT_SOCKET` is set.
   - It uses a Unix socket with mode 0600 in `$XDG_RUNTIME_DIR`, never TCP. On a shared server, an eval endpoint is
     code execution as the user.
   - On Windows, use a named pipe with an owner-only ACL.
2. **Snapshot before mutate**: Gaia's `snapshot` is called before every eval, advance and set_advancing, so undo
   restores world state. The check verifies this is pixel-identical. Undo does not cover:
   - files
   - FSI definitions
   - static mutable state
   - imperative-mode projects, where Gaia disables undo (the tool should refuse or warn there)
3. **Containment of later failures**: Gaia already rewinds and halts on exceptions thrown inside the frame loop.
4. **Liveness** (hardened in round 2; verified by `check_mcp.py`). A non-terminating eval blocks the editor thread
   and cannot be cancelled in-process (FCS runs the compiled code synchronously; .NET has no thread abort). So the
   transport contains it instead of pretending to cancel it:
   - Each request has a timeout (default 120 s; `timeout_s` 1–600 per request). On expiry the request is
     **withdrawn** through an atomic queued→claimed/withdrawn handoff, so a request whose client gave up can never
     run later. The reply says whether the request had already started, in which case its effects may still land.
   - The editor thread writes a heartbeat every frame and records the op it is running. `ping` is answered on the
     connection thread, so it works while the editor is blocked.
   - After 10 s without a frame, new requests **fail fast** with an error naming the running op instead of
     queueing behind it. A legitimately long op such as `reload_code` (~25 s) also trips this, and the error says so.
   - Recovery is a supervisor restart (`gaia_restart`), which loses unsaved world state. It signals only a process
     whose cmdline is `Nu.Gaia.dll` **and** whose environment carries this server's socket path. Gaia ignores
     SIGTERM (SDL turns it into a quit event), so the restart escalates to SIGKILL after 5 s. The socket file is then
     left behind and is removed as stale at the next start, after a connect probe shows nobody is listening.
   - Gaia's own "Unhandled Exception!" dialog (an exception in the frame loop) no longer stalls the bridge.
     `status` and `ping` report `pendingException`, most ops refuse with a pointer, and `gaia_resolve_exception`
     (hard_rewind | ignore) resolves it headlessly.
   - Time control is serialized (one advance/set_advancing in flight) and `frames` is capped at 600 on the server.
     A failing deferred completion is caught, so an advance's halt can never be skipped by an exception.
   - The transport never blocks the accept thread. Each connection runs on the thread pool, waits are async
     (`WhenAny`), in-flight requests are capped at 64 and connections at 16, and accept errors are logged instead of
     killing the editor. The bridge's bookkeeping (log journal, heartbeat) is skipped entirely when it is off.
   - The client checks that reply ids match request ids and drops a connection after any timeout, so a stale reply
     is never read as the answer to a later request (research-workflow-evaluation lesson 3).
5. **Local access control** (round 2, from the review). The socket must live in a private directory: mode 0700,
   owned by the user, not a symlink, i.e. `$XDG_RUNTIME_DIR`. Gaia refuses to start the bridge otherwise, and the
   client checks the socket's owner before sending code. There is no `/tmp` fallback anywhere. The launcher's log
   directory (whose `gaia.project` decides what `gaia_restart` runs) gets the same checks. Xvfb runs with an
   Xauthority cookie, so other local users cannot screenshot the display or inject input.
6. **Isolation for untrusted or autonomous runs**:
   - Run Gaia in a container or VM with no network, on a git worktree of the project.
   - The agent's file edits are then git-revertible.
   - Network isolation also prevents the copy-from-the-internet effect seen in A4.
7. **Human in the loop**: MCP's guidance (E1) is that clients should let users deny tool calls. `gaia_eval` and
   `gaia_reload_code` are the calls to gate in cautious modes. Read-only tools need no gate.

### 5a. Lessons from research-workflow-evaluation (exchange note, 1 Nov) and how this design applies them
These are that project's operational findings about agent harnesses, not literature evidence. Source:
`~/workspace/exchange/research-workflow-evaluation--to--nu-gaia-agentic.md`, §3.
| Lesson | Applied here |
|---|---|
| Qualify real execution, not configuration text | `check_mcp.py` drives a live Gaia through a real MCP round trip and exercises a compile error, a runtime error, a timeout (deliberate hang) and a recovery. Nothing is claimed from configuration alone. |
| Guards give false positives; keep raw evidence apart from the gate | No textual eval filter (e.g. rejecting `while true`): it would block harmless code and miss real hangs. Liveness is gated on the observed heartbeat, and `ping` returns the raw fields (frame, ms since frame, running op) beside the `unresponsive` verdict. |
| Route events by request id; never merge stale completions | Atomic withdraw-on-timeout in the transport; client-side id check; a connection is dropped after any timeout (§5.4). Deferred replies (advance, wait_frames) are bound to their own request object, not to "the current request". |
| Own and clean up spawned processes | Partly done. `gaia_restart` signals only a pid whose cmdline is `Nu.Gaia.dll`, and the launcher detaches stdin and records pid and project. Not done: a cgroup-scoped owner (e.g. `systemd-run --user --scope`) that would certify that the whole process tree is gone. That is next if Gaia is ever run unattended. |
| Independent code-only review for safety-relevant code | Done in round 2. A separate Sonnet reviewer, given only the code and the contract, found 15 issues (2 blocking: an accept-loop exception would kill Gaia, and the first request could run on the accept thread). All were fixed or documented (§5.4, §5.5, §8). As in that project, passing author checks (26/26) had not caught them. |
| Check the tool inventory the agent actually sees | Done in round 2. A real `claude -p` session (Haiku, `--strict-mcp-config`, 5 turns, $0.005) listed all 16 `mcp__gaia__*` tools and called status, eval and screenshot. It described the screenshot correctly (the Nu splash logo) and exposed a regression: init noise leaked into the eval output after a symbol-first session, which is now fixed and checked. Record: `prototype/claude-code-session-check.json`. |
| Replay saved code to check claimed numbers | The check is a script with a saved transcript (`prototype/check-transcript.json`), so every number in §8 can be replayed. |

## 6. Alternatives and why not (details in `investigation.md` §2)

- **Terminal REPL only**:
  - Pros: simplest, and works with any shell agent. It ships as `gaia_bridge.py repl`.
  - Cons: no first-class images, so the agent must open saved PNGs. Every structured query becomes hand-written
    F#. Executable actions do beat JSON actions in general (C8), and `gaia_eval` keeps that power. Recommended as
    the secondary client, not the only one.
- **MCP with a single `eval` tool**:
  - The agent must already know the Nu API to inspect anything, and output is unstructured.
  - The orient/observe tools are cheap and cut wasted rounds.
- **GUI computer-use on the Gaia window**: pixel-only grounding is the weakest channel (D7, D8). It is useful only
  as a last-resort playtester.
- **Language server beside Gaia (FsAutoComplete, FsLangMcp)**:
  - It cannot see the live session, session-defined types or hot-reloaded types.
  - Reuse its techniques instead: TipFormatter, SignatureHelp, SourceLink.
  - It complements Gaia for multi-file refactors.
- **Stub-world headless runs** (`World.makeStub`): good for CI-style logic assertions, but they produce no pixels,
  so they don't support design.
- **NVIDIA GPU rendering on this server**: it would need a real X server or an offscreen Vulkan surface, which means
  root or engine changes. Lavapipe is adequate for 2D and light 3D, so this is deferred.

## 7. How to evaluate it (from the literature, §G)

- **Task suite**: ~20 Nu design tasks across Breakout, Blaze Vector and Sand Box 2d (layout, tuning, new mechanic,
  visual polish). Each task has:
  - a deterministic oracle: state assertions over N frames, mutated to validate the oracle itself (A4);
  - a rubric judged on captured frame sequences by a separate model that has not seen the agent's trace (A2, A8).
- **Ablations** against the same agent and budget, each adding one channel:
  1. code-gen blind (status quo)
  2. + eval/diagnostics
  3. + inspect/symbol
  4. + screenshot
  5. + frames
- **Measures**: success, rounds, tokens, wall time, regressions across turns (A5), and undo use.
- **Hygiene**: network-isolated runs (A4).
- **Small within-subject human study** (designers using Gaia with and without the agent): time to goal, rollbacks,
  perceived control, attention load (B1, A16). This is the weakest evidence area, so it is the most informative
  study to run.

## 8. Prototype status and reproduction

- **Code changes** (branch `agentic-gaia`, local only):
  - `Nu/Nu.Gaia/GaiaAgentBridge.fs`: the transport.
  - `Nu/Nu.Gaia/Gaia.fs`:
    - the eval body is factored into `evalInteractive`, which the Interactive window now also uses;
    - the `agent*` ops and the `agentProcess` hook in `imGuiProcess`;
    - the fix for dropped first-eval output on Linux.
- **Reproduce**:
  ```bash
  nice dotnet build Nu/Nu.Gaia/Nu.Gaia.fsproj -c Debug        # builds engine + sample projects (~5 min first time)
  docs/agentic-gaia/prototype/run_gaia_headless.sh             # Xvfb :91 + lavapipe + Breakout ImSim + bridge
  python3 -I docs/agentic-gaia/prototype/check_mcp.py OUT_DIR  # 28 checks incl. hang + restart (NU_GAIA_CHECK_HANG=0 skips); PNGs + transcript
  claude mcp add gaia -- env NU_GAIA_AGENT_SOCKET=$XDG_RUNTIME_DIR/nu-gaia-agent.sock DISPLAY=:91 \
      NU_XML_DOC=$PWD/Nu/Nu/bin/Debug/net10.0/Nu.xml python3 $PWD/docs/agentic-gaia/prototype/gaia_mcp.py
  ```
- **Evidence**:
  - `prototype/check-transcript.json` (every MCP request and reply, with timings)
  - `img/check-1-before.png`, `check-2-after.png` (an eval-created sprite; 21.9% of pixels changed),
    `check-3-undone.png` (identical to before)
  - `img/check-4-frames.png`
  - `img/gaia-headless-editor.png`
- **Not verified**:
  - a source-edit + reload cycle that changes behaviour (reload was run without source changes and succeeded);
  - Windows named pipes;
  - 3D scenes under lavapipe;
  - Gaia's exception dialog path (`gaia_resolve_exception`) and time-control serialization: implemented and
    reviewed, but not exercised by `check_mcp.py` (no reliable way yet to make a frame throw on demand).
- **Spike-quality gaps**:
  - Fixed in round 2: screenshots are cropped to the window rectangle that `status` reports (SDL position and
    size), with no root-window margin. The first `symbol` call no longer adds an undo entry, because session
    initialisation was factored out of `evalInteractive`. Hung evals are contained (§5.4) but still cannot be
    cancelled.
  - A request withdrawn on timeout stays in the queue (counted by `ping.queued`) until the editor thread drains
    and skips it.
  - Python is used for the MCP side; F# or C# would remove the second runtime.
  - Cost on a shared server: the idle headless editor used about 7 CPU cores (`ps` showed 701% after 3.7 min),
    likely because lavapipe renders every frame in software and Gaia runs with `FramePacing = false` (cause not
    isolated). In round 2 the launcher sets `LP_NUM_THREADS=4`, and the idle cost fell to about 3.7 cores
    (`top`: 366%). Stop Gaia when the agent is idle, or add frame pacing or a render-on-request mode for agent
    sessions.

## 9. Next steps (in order)

1. **In-engine frame readback**: in `VulkanContext.endFrame`, add `vkCmdCopyImageToBuffer` from `resolveTexture`
   to a staging buffer, then add a `capture` op. This gives exact frames with no X, and real clips.
2. **More tools**: `gaia_diff`, `gaia_assert` and `gaia_checkpoint`, which supply the verification signals the
   literature says matter most.
3. **`gaia_save` and `gaia_set_property`**, using Gaia's existing converters. This makes the persistence
   boundary explicit.
4. **Imperative-mode guard; FSI-only `gaia_reset_session`.** The watchdog and supervisor restart were done in round 2.
5. **FCS entity index for `gaia_search_api`**; SourceLink fallback for NuGet installs.
6. **Run the §7 ablation** on a handful of tasks before investing in polish.

Upstreaming should only be considered if the user asks; this branch is local and no PR has been opened.
