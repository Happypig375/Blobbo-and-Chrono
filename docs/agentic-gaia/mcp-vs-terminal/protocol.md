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
