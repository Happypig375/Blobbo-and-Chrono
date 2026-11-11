# MCP vs a plain terminal command for Gaia's agent tools: report (11 Nov 2026, 03:13 HKT)

**In short.** A completed pilot **with protocol deviations** (section 2): not exact compliance with the frozen
protocol, and not broad acceptance of either route. The orchestrator's outcome review (11 Nov) checked the
artifacts: 12/12 oracle passes and 58/58 live checks.
- Python is now optional. A bash command, `gaia`, uses only bash, netcat and coreutils. It works against a live
  Gaia on the NVIDIA GPU: 58 of 58 live checks pass.
- Its screenshots are pixel-identical to the MCP server's, and the same assert gives the same result through
  either route.
- In a matched pilot (2 tasks × 3 runs × 2 routes, Opus 5.5), **all 12 runs passed**. So this pilot shows no
  difference in success.
- The MCP route was a little faster and used fewer tokens:
  - median time: bounce 65 s vs 74 s, rows 51 s vs 77 s;
  - whole-run prompt tokens: 168k vs 205k (bounce) and 108k vs 238k (rows).
- Connected but never used, MCP costs less context than the terminal route (+1,072 vs +2,520 prompt tokens), because
  Claude Code defers MCP tool definitions by default.
- Two tasks and three runs per route are a pilot. They do not support a general claim that either route is better.

Terms:
- **MCP** (Model Context Protocol) is the standard way an agent gets tools from a separate server process. Here that
  process is `gaia_mcp.py` (Python).
- **Terminal route**: the agent runs `gaia ...` through its Bash tool.
- **Bridge**: the socket inside Gaia that both routes talk to. It is unchanged.
- **Prompt tokens**: input + cache-creation + cache-read tokens. This is the whole prompt, summed over every turn of a
  run.

## 1. Measured

### 1a. Connected-but-unused cost (idle context; one call each; same prompt "Reply with exactly the word OK")

| Configuration | Prompt tokens | Extra over file tools only | Folder |
| --- | --- | --- | --- |
| I0: file tools only | 6,145 | 0 | idle-run2 |
| IB: + Bash tool, no `gaia` text | 7,694 | +1,549 | idle-run3 |
| IT: + Bash tool + `gaia` usage text and advice | 8,665 | +2,520 | idle-run2 |
| IM: MCP, tool search on (Claude Code default) | 7,217 | +1,072 | idle-run2 |
| IMF: MCP, tool search off (`ENABLE_TOOL_SEARCH=false`) | 9,481 | +3,336 | idle-run2 |

- **Split of the terminal cost** (now measured, not estimated): the Bash tool is +1,549 and the `gaia` usage text
  is +971. Phase 1 estimated the text at about 600 tokens from its length; that estimate was too low.
  - An agent that already has Bash pays only the +971.
  - An agent without a shell pays both.
- **Deferred loading:** with tool search on, the 19 MCP tool definitions do not enter the prompt; only their names,
  the server instructions and the `ToolSearch` tool do. Turning tool search off adds all 19 up front.
  - Checked against Claude Code's current docs ("Scale with MCP tool search").
  - In the pilot, every MCP run actually loaded tools through `ToolSearch`: 1–2 searches per run, 7 in total.
  - **Advertised, discovered, loaded:** the server advertises 19 tools (`tools/list`, 7,528 bytes of JSON), and all 19
    are listed by name at session start. Each pilot run then loaded only 4–6 full definitions, chosen by name
    (`select:` queries for reload_code, eval, assert, status, advance and once hierarchy). The other 13–15 were
    never loaded. The terminal route has no such step: its whole usage text is in the prompt from the start.
- Same Claude Code version (2.1.322) in idle-run2, idle-run3 and the pilot.

### 1b. Live check of the `gaia` command (Gaia on the NVIDIA GPU, Vulkan, own Xvfb)
- `prototype/check_gaia_cli_live.sh`: every subcommand, with the expected exit codes written in the script before
  running.
  - Final run: **58 of 58 pass** (`live-check/cli-check3.log`).
  - Coverage: ping, status, eval (value, quotes/backslashes/tabs, stdin, compile error, runtime error), hierarchy,
    inspect, advance, checkpoint save/restore/delete, diff, assert (pass, fail, no-advance, compile error), capture
    (editor and game view), frames, undo, redo, reload_code, log, symbol, docs, raw, unknown command, bad number,
    restart.
- **Image parity** (`prototype/check_capture_parity.py`): game time halted; editor and game views; at the splash
  screen and in gameplay.
  - The CLI capture and MCP `gaia_screenshot` (`max_width=0`) are **pixel-identical**: 0 differing pixels.
  - Two CLI captures in a row are identical too, as a control.
  - The images are 1280×720 on this display, so MCP's default 1280 px limit changed nothing. MCP's `gaia_frames`
    sheet for 3 frames is 1440×270 (480 px per frame).
  - I looked at one gameplay frame myself: bricks, ball, paddle and score are drawn correctly
    (`live-check/parity-gameplay/game-cli-1.png`).
- **Behaviour parity:** the same `assert` from the same checkpoint gave the same reply through both routes: passed,
  frames checked, failing frame, update time and probe value (`live-check/parity-gameplay/parity.json`).
  - My predicate happened to fail at frame 1 in both. So this shows that the two routes agree, not that a passing
    multi-frame assert was compared.

### 1c. Task pilot (protocol condition 2; `pilot/`; Opus 5.5; 6 runs per route)
Both oracles were re-validated on the GPU first (`pilot/oracle-validation.json`): the untouched code fails and the
reference fix passes, for both tasks.

| Task, route | Passed | Median time | Median turns | Median tool calls | Median prompt tokens | Median output tokens |
| --- | --- | --- | --- | --- | --- | --- |
| bounce (bug fix), MCP | 3/3 | 65 s | 13 | 12 | 167,675 | 2,354 |
| bounce, terminal | 3/3 | 74 s | 13 | 12 | 204,625 | 2,662 |
| rows (feature), MCP | 3/3 | 51 s | 10 | 9 | 108,262 | 1,460 |
| rows, terminal | 3/3 | 77 s | 15 | 14 | 237,922 | 2,982 |

Per-run numbers: `pilot/analysis.txt`, `pilot/summary.json`. Raw records: `pilot/results.jsonl`. Full agent
streams and diffs: `pilot/runs/`.

- **Success and quality.** 12 of 12 passed the automatic oracle, which checks the game's behaviour after loading
  the saved sources into a fresh Gaia. Both routes passed everything, so this pilot cannot rank them on success.
  Quality beyond the oracle (diff size, style) was not scored; the diffs are kept.
- **Time and tokens.** The MCP route was faster and used fewer tokens in every pairing:
  - median time 65 vs 74 s (bounce) and 51 vs 77 s (rows);
  - prompt tokens about 1.2× and 2.2× as many on the terminal route.
  - Most prompt tokens are cache reads: the agent re-reads its conversation every turn. Terminal-route tool output
    is raw bridge JSON (MCP trims some fields), and terminal agents pipe it through `head` or `grep`, so their
    turns carry more text.
  - The reported notional cost was $0.84 for MCP and $1.23 for terminal, all 12 runs on the included account. This
    figure is computed by Claude Code, not billed.
- **How the routes were used.**
  - MCP: 3–7 Gaia tool calls per run, mostly eval, assert, reload_code, advance and status.
  - Terminal: 9–13 shell calls per run, chaining at least 14–22 `gaia` invocations. This is a floor: the commands
    were stored cut at 200 characters.
- **Images.**
  - No MCP run took a screenshot.
  - One terminal run (rows-T-r3) made 4 captures and looked at 1 of them; counted from the image blocks actually
    returned to the agent.
  - Neither route needed images for these tasks: assert and eval did the checking.
- **Startup.** The first stream line arrived after 0.7–0.9 s in both routes. This measures only the time to Claude
  Code's init line, not the MCP server's handshake on its own.
- **Dependencies.** MCP route: Python 3 and Pillow, plus a long-running server process per agent. Terminal route:
  bash, netcat (OpenBSD), base64 and coreutils, and no extra process.

## 2. Deviations, failures and fixes (all kept)
- **The terminal route was not shell-free.** `Bash(gaia:*)` does not block Claude Code's built-in read-only commands.
  - Terminal agents also ran `ls`, `cat`, `grep` and `head` (2–6 such commands per run).
  - Writes and loops were denied: `sed -i` and `for` loops, 5 denials in all.
  - These reads give no access that Read, Glob and Grep don't already give both routes. Still, the frozen protocol
    said "no other shell command" (protocol amendment 5).
- **Prompt wording asymmetry (known before running).** The unchanged task prompt mentions "tools named gaia_*".
  The terminal route learns `gaia` from the appended system prompt instead.
- **Live-check bugs found and fixed in the CLI.** The stand-in bridge test had missed both.
  - The bridge's JSON encoder writes "+" in the base64 PNG as `+`, so capture failed to decode.
  - Bash's own pattern matching on the 0.5 MB capture reply took 86 s per capture. With grep and prefix checks it
    takes 0.57 s and gives a byte-identical PNG.
  - The first two check runs are kept: `live-check/cli-check.log` has 45 pass and 13 fail; `cli-check2.log` has
    58 pass but slow.
  - Three of the first failures were my test expectations, not the CLI:
    - JSON writes a quote as `"`;
    - `failwith` needs a type;
    - the property is `GetExists`.
  - The first restart failed because my environment lacked `GAIA_GPU_SHARE=1`.
- **Harness failures:**
  - The first pilot start used a relative path, the same mistake as idle-run1, and the first run never reached the
    model (`pilot-failed-start1/`).
  - A crash on HTTP 429 from the usage endpoint lost no run.
  - Two runs used a 4–8-minute-old quota reading.
  - The image counters in the harness were wrong (one missed chained captures, one double-counted a PNG Read); the
    analysis recounts from the streams.
  - The protocol notes one claimed restart that did not happen, and corrects it (amendments 4–7).
- **The GPU gate was made concrete** (amendment 2). An idle labmate service, `buildinggpt-rag` (4.6 GB, 0 %), is
  always on the GPU, so launches use `GAIA_GPU_SHARE=1` after `gpu_gate.sh` checks three things:
  - no other Gaia is running;
  - utilisation is under 10 %;
  - no other GPU process is present.

  At first the gate wrongly counted a Nu-value CPU build (amendment 3). The log is `gpu-gate.log`.
- **Quota:** weekly use was 90 % before this allocation and 91 % before and after the pilot. Five-hour use went from
  6 % to 17 %.
  - Model calls this allocation: 1 idle call (IB) and 12 pilot runs, with no failed model calls. The failed first
    start never reached the model.

## 3. Exploratory (not measured claims)
- The token gap between routes looks like it comes from verbose tool output plus more turns on the terminal
  route, not from definitions. Idle cost differs by only about 1,450 tokens, while whole runs differ by
  37k–130k. A terminal output mode that trims the JSON like MCP does might close much of the gap. This was not
  tested.
- Terminal agents chained many `gaia` calls into one shell call. This needs fewer tool calls per bridge operation,
  but did not make the runs shorter here.

## 4. Still needed
1. **A harder or more varied task set.** Both of these tasks hit the ceiling (12/12), so the pilot cannot tell the
   routes apart on success. Tasks where the 2 Nov study saw failures are candidates, or tasks that need images.
   These must be chosen and frozen before running.
2. **More runs per route** to tell a real time or token difference from run-to-run noise. Three per cell is a
   pilot.
3. **A strictly shell-free terminal route** (a hook or deny rules for every non-`gaia` command) if that matters for
   the claim.
4. **Optional:** a terminal output mode that trims replies like MCP, then compare tokens again.
5. **Wiki publication (approved; blocked by a sign-in step).** Re-checked at about 03:11 HKT (one check; not repeated):
   - The fork's wiki is switched on, but its git repository answers "Repository not found" (`git ls-remote` and a
     dry-run push).
   - GitHub creates that repository only when a first page is saved on the website, and it has no API for wiki
     pages. The `gh` login can't do it, and the server's browser is signed out of GitHub.
   - So a signed-in person must click "Create the first page" once; NEEDS-HUMAN.md has the steps. Publication
     itself is already approved (D4); no further approval is needed.
   - Meanwhile the new results are a page in the prepared local wiki (`~/workspace/nu-gaia-agentic-wiki`, one local
     commit on top of the unchanged `f1e1adb`; its hash is in NEEDS-HUMAN.md), so one push publishes everything.
   - Its file links point at the `agentic-gaia` branch on the fork. Update, 11 Nov evening: that code was reviewed
     (CLI fixes, numbers against `pilot/summary.json`, parity files, timestamps; offline CLI check 18/18) and pushed
     to the fork's `agentic-gaia` branch as a fast-forward. Before the push, the published copy of `gpu-gate.log` was
     redacted: three lines had quoted other processes' full command lines (other projects' sessions); only their PID
     and program name are kept.

All items in the frozen protocol are done. Items 1–4 are new experiments; they need a new protocol and time
allocation, not a decision. Item 5 waits only on the one-time first wiki page (a GitHub sign-in).
