# Protocol: Gaia agent tools through MCP vs a plain terminal command (frozen 11 Nov 2026, 00:27 HKT)

Frozen before any comparison outcome was seen. Changes after this point go in a dated "Amendments" section with
the reason; the frozen text stays.

## Question
Can an agent use Gaia's agent bridge as well through a plain terminal command (`gaia`, bash + netcat, no Python)
as through the MCP tool server (`gaia_mcp.py`, Python), and what does each cost in context and time, including
when the tools are connected but never used?

Terms: **MCP** (Model Context Protocol) is the standard way the agent gets tools from a separate server process.
**Terminal route** = the agent runs `gaia ...` with its Bash tool. **Bridge** = the Unix socket inside Gaia that
both routes talk to (unchanged). **Tool search / deferred loading** = Claude Code sends only MCP tool names (and
server instructions) at start and loads a tool's full definition when the agent searches for it.

## Arms (identical except for the route)
- Same Claude Code version (record `claude --version`), model `claude-opus-5-5`, default effort, the included
  Claude account only. `--setting-sources project,local`, `--no-session-persistence`, `--strict-mcp-config`,
  `--permission-mode acceptEdits`, auto-memory folder cleared before each run (as `ablation/run_ablation.py`).
- **M (MCP):** tools `Read,Edit,Write,Glob,Grep,ToolSearch` + `mcp__gaia` (all tools, server instructions on,
  engine capture). No shell.
- **T (terminal):** tools `Read,Edit,Write,Glob,Grep,Bash` with Bash allowed only as `Bash(gaia:*)` (no other
  shell command). The CLI's usage text and the same advice as the MCP server instructions are given through
  `--append-system-prompt` (the terminal equivalent of server instructions). Images: `gaia capture/frames` write
  PNG files; the agent must `Read` them to see them (MCP returns them inline). Record whether each image was
  actually viewed (a `Read` of the PNG in the stream), not just produced.
- **Not matched exactly, recorded instead:** MCP `gaia_frames` returns one contact sheet; `gaia frames` returns
  separate files. MCP downsizes screenshots to 1280 px; the CLI keeps the renderer's size. `nu_docs` ranking
  differs (grep vs scored). `gaia_screenshot` X-grab mode is not in the CLI (MCP's default is engine capture).

## Conditions
1. **Idle context (no Gaia needed, no GPU):** prompt "Reply with exactly the word OK and nothing else." One
   turn, no tools used. Four configurations, one call each, run in this order:
   I0 = file tools only (no MCP, no usage text); IT = arm T config (Bash + usage text);
   IM = arm M config (tool search default); IMF = arm M with `ENABLE_TOOL_SEARCH=false` (all schemas upfront).
   Record from stream-json: the init message (tools list, MCP server status), and the result usage
   (input, cache creation, cache read, output tokens), wall time, cost field. Also, offline: bytes of the MCP
   `tools/list` reply and of the instructions; bytes of the T usage text.
   Instrumentation limit: one call per configuration cannot separate small differences from prompt-cache noise;
   cache creation + cache read + input is the total prompt size and is what is compared.
2. **Task pilot (needs Gaia on the GPU; waits for the GPU gate):** tasks `bounce` (the hard bug fix) and
   `rows` (a feature task), from `ablation/tasks.py`, unchanged prompts and the existing oracles (scoring
   unchanged). Initial state: fresh copy of the same Breakout ImSim work tree and a freshly launched Gaia per run.
   Engine revision: branch `agentic-gaia` at the commit recorded in each run. 3 runs per arm per task, arms
   alternated M, T, M, T... Each run: at most 60 turns, 20 minutes. Measure success (oracle), wall time, turns,
   tool calls by kind, tokens (input/cache creation/cache read/output), whether images were viewed, and the
   agent's final diff (kept). Fresh context per run; no transcript, result or note from one run is visible to
   another.

## Validation before the pilot
- CPU-only: `prototype/check_gaia_cli.sh` (stand-in bridge) passes.
- On the GPU, when allowed: every `gaia` subcommand against a live Gaia, mirroring `check_mcp.py` coverage
  (ping, status, eval ok/compile error/runtime error, advance, hierarchy, inspect, checkpoint/diff/assert,
  capture and frames give valid PNGs identical to MCP engine capture of the same frame, reload_code, log,
  symbol, docs, restart).

## Budget and stop rules
- Refresh quota (`read_quota.py --main`) before each costly stage; stop if the stage would push weekly use past
  100% or the five-hour window past 80%, or on any provider limit. No model substitution.
- This phase (checkpoint 00:45 HKT 11 Nov): at most one sequential experiment = condition 1 (four short calls).
- GPU gate: no Gaia run while Nu-value's GPU trial is active (their `.orchestration/*/latest.txt` says rounds
  running, or their Gaia/Nu process or Xvfb is up). Own Xvfb, NVIDIA Vulkan only, `nice`; no software rendering.
- Do not change tasks, prompts, scoring or arms after seeing outcomes. Report failures and null results as they
  are. One or two tasks cannot support a broad efficiency claim; lower context use is not evidence of better work.
- Old results (`ablation/`, 2 Nov) are baseline evidence and are not rewritten.

## Amendments
- 11 Nov 00:27 HKT: `idle-run1` failed (harness used a relative output path after `cd`; the MCP config was not
  found, the model was never called for IM/IMF). Fixed the path; all four configurations rerun unchanged as
  `idle-run2`. No prompt, arm or measure changed.
- 11 Nov 02:30 HKT, **amendment 2, before any live run, Gaia launch or task-pilot outcome** (new allocation: 90 active
  minutes; Nu-value's GPU trial checkpointed at 16:40 UTC 10 Nov):
  - **GPU gate, made concrete** (`gpu_gate.sh`, checked before every Gaia launch, logged to `gpu-gate.log`): no Nu or
    Gaia process from any project; GPU utilisation under 10 % on two polls 2 s apart (the rule Nu-value adopted on
    10 Nov); the only other GPU process is the resident, idle `buildinggpt-rag` service (4.6 GB, 0 %, up 3 days).
    Because that service is always on the GPU, the launcher's own "no other process" check can never pass, so
    launches use `GAIA_GPU_SHARE=1` after this gate. Still NVIDIA Vulkan only; no software rendering.
  - **Live CLI check:** `prototype/check_gaia_cli_live.sh` (every subcommand; expected exit codes written in the
    script before running) and `prototype/check_capture_parity.py` (game time halted; CLI capture, then MCP
    `gaia_screenshot` with `max_width=0`, then CLI again as a control; editor and game views; exact pixel compare).
  - **New idle control IB** (Bash tool, no usage text, otherwise as IT), one call, same prompt and flags, run after
    the live check. It splits IT's +2,520 into the Bash tool and the usage text. Results go to `idle-run3/`.
  - **Task pilot harness** (`run_pilot.py`), fixing details the frozen text left open:
    - Order: rep 1..3, and within each rep bounce-M, bounce-T, rows-M, rows-T (global M, T alternation).
    - Work tree: a fresh copy of `Projects/Breakout ImSim` (built at the current sources) outside the repo;
      sources restored from the repo before every run (as `run_ablation.py`). Fresh Gaia per run; oracle on a
      second fresh Gaia (as `run_ablation.py`). Prompts: `tasks.prompt(task)`, unchanged in both arms. That prompt
      says "If tools named gaia_* are available ..."; arm T has no such tools and learns `gaia` from the appended
      system prompt. This wording asymmetry is recorded, not fixed.
    - Arm M: `gaia_mcp.py` with all 19 tools, server instructions on (`NU_GAIA_INSTRUCTIONS=1`), engine capture.
    - Arm T: the exact `idle-run2/terminal-usage.txt` as `--append-system-prompt`; `gaia` on PATH; Bash allowed only
      as `Bash(gaia:*)`; `--add-dir` for the screenshot folder so the agent can `Read` the PNGs.
    - Both: Opus 5.5, `--max-turns 60`, 20-minute wall limit, no `--max-budget-usd` cap (the 2 Nov Haiku runs used
      $1; an Opus run on the included account could exceed that notional figure, and the frozen limits are turns
      and time), web, subagents, notebooks and todo tools disallowed, auto-memory cleared before each run.
    - Recorded per run, beyond the frozen list: permission denials (shell commands outside `gaia`), images produced
      vs images actually seen (M: image blocks returned inline; T: `Read` of a `.png`).
    - Quota read before every run; stop at weekly 100 %, five-hour 80 %, any provider limit, or when the remaining
      active time cannot fit a run plus the write-up (about 10 minutes reserved). Partial pilots are reported as
      partial, with the runs completed.
- 11 Nov 02:29 HKT, amendment 3 (apparatus only): `gpu_gate.sh` first refused because Nu-value was running a
  CPU-only `dotnet build` (their allocation is CPU/oracle work first). The exclusive resource is the GPU, so the gate
  now counts any running Gaia (`Nu.Gaia.dll`) and any GPU process other than the idle `buildinggpt-rag` service;
  CPU builds do not close it. Live-check findings before this point are kept in `live-check/cli-check.log`.
- 11 Nov 02:41 HKT, amendment 4 (apparatus only): the first pilot start failed the same way as idle-run1. The harness
  was given a relative output folder, so the MCP config path was wrong from inside the work tree and the first run
  (bounce-M-r1) exited before reaching the model. Stopped at once (its oracle had not finished; no record written);
  kept as `pilot-failed-start1/`. Paths made absolute; the pilot restarts from bounce-M-r1, nothing else changed.
- 11 Nov 02:44 HKT, amendment 5 (deviation found while bounce-T-r1 was running, before its outcome): `Bash(gaia:*)`
  does not stop Claude Code's built-in read-only shell commands. The agent's first command, `ls; grep -n -i ...`,
  ran without a permission prompt or denial, so arm T can also read files through the shell. Arm M (and T) already
  have Read, Glob and Grep, so this adds no new kind of access, but the frozen text ("no other shell command") is
  not what ran. The pilot continues unchanged (stopping would lose the matched set within the time allocation);
  every run's non-`gaia` shell commands are counted and reported. A stricter T arm (a hook or deny rules) is
  future work.
- 11 Nov 02:51 HKT, amendment 6 (apparatus only): after 5 runs the harness crashed before bounce-T-r2 because the
  usage endpoint answered HTTP 429 (the quota reader was rate-limited; this is not a model or account limit). No run
  was lost or started. The quota read now retries 4 times, 30 s apart, and otherwise uses the last good reading if
  it is under 20 minutes old (recorded as `stale_s`); with no usable reading the pilot stops. Restarted at
  bounce-T-r2; finished runs are skipped.
  Budget stop tightened at the same time (no outcome involved): no new run once weekly use reaches 93 % (this
  project's forecast share was 2 points from 90 %, plus 1 point of the shared lag), so Nu-value's share is kept.
  Weekly use was 90 % before the pilot and 91 % before its fifth run.
  **Correction (03:08 HKT):** the restart described in amendment 7 did not happen. My restart condition (no Gaia
  running and the harness waiting on the quota read) never held at the moments I checked, so the original process
  ran to the end with its older quota logic. Runs from bounce-T-r2 onward used the last good main-login reading
  (taken 02:48, 91 % weekly) when the endpoint answered 429, and every record states the reading's age (`stale_s`).
  The project-login fallback was never used in the pilot.
  Second correction (03:09 HKT, from the records): only bounce-T-r2 and rows-M-r2 used the older reading (263 s and
  495 s old); the main login answered again from bounce-M-r3... onward is wrong too: from rows-T-r2 onward every
  reading was fresh (`stale_s` 0). Weekly use read 91 % before every run.
  Clean statement of the above (the second correction's wording is muddled): runs 1-5 had fresh readings; bounce-T-r2
  and rows-M-r2 used the 02:48 reading (263 s and 495 s old); rows-T-r2 to rows-T-r3 had fresh readings.
