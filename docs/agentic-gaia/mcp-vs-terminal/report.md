# MCP vs plain terminal command for Gaia's agent tools: phase 1 report (11 Nov 2026, 00:30 HKT)

**In short.** Python is now optional for using Gaia's agent tools. A new command, `gaia`, uses only bash, netcat
and coreutils, and talks to the same bridge inside Gaia. It covers every bridge operation that the MCP server
(`gaia_mcp.py`, Python) offers. It passes an 18-point check against a stand-in bridge, but it has **not yet been
run against a live Gaia**: that needs the GPU, which is reserved by Nu-value's trial. The first measurement, the
cost of connected-but-unused tools, is done:

| Configuration (one `claude -p` call each, Opus 5.5, same prompt) | Prompt tokens | vs file tools only |
| --- | --- | --- |
| I0: file tools only | 6,145 | 0 |
| IT: terminal: + Bash tool + `gaia` usage text and advice | 8,665 | +2,520 |
| IM: MCP, tool search on (Claude Code's default) | 7,217 | +1,072 |
| IMF: MCP, tool search off (`ENABLE_TOOL_SEARCH=false`) | 9,481 | +3,336 |

The task comparison (does the agent do the work as well?) has not been run. Nothing here says one route gives
better results.

## Measured
- **Idle context** (protocol condition 1, folder `idle-run2/`, summary in `idle-run2/summary.json`). "Prompt tokens"
  means input + cache creation + cache read. This is the whole prompt, so it does not depend on cache luck. It is
  fixed for a given configuration, and one call per configuration measures it. All four calls replied `OK` in
  4.2–4.9 s, using 4 output tokens each.
  - **MCP schemas do not all enter the prompt by default.** With tool search on (the default), the init message
    lists all 19 `mcp__gaia__*` tools as available, but only their names, the server instructions and the
    `ToolSearch` tool definition go into the prompt (+1,072). Turning tool search off puts all 19 definitions in
    upfront (+3,336, and `ToolSearch` disappears from the tool list). This matches the current Claude Code docs
    ("Scale with MCP tool search": "Only tool names and server instructions load at session start"; tool search
    is on by default; `ENABLE_TOOL_SEARCH=false` loads all upfront). Claude Code version: `idle-run2/claude-version.txt`.
  - **The terminal route's cost is mostly the Bash tool, not the `gaia` text.** The usage text plus advice is
    2,461 characters, roughly 600 tokens (estimated from characters, not measured). The rest of the +2,520 is the
    Bash tool's own definition. An agent that already has Bash for other work pays only the usage text. One
    with no shell otherwise, like the 2 Nov study's agents, pays for both. A separate "Bash without usage text"
    call would split the two exactly; it was not run in this phase (limit of one experiment).
  - Offline sizes: the MCP `tools/list` reply is 7,528 bytes of JSON for 19 tools; the server instructions are
    381 characters.
  - The old 2 Nov study gave its agents `ToolSearch`, so its MCP tools were most likely deferred too (not checked
    in the old streams).
- **CLI check** (`docs/agentic-gaia/prototype/check_gaia_cli.sh`, CPU only): 18/18 pass. It checks the exact
  request lines (escaping of quotes, backslashes, newlines and tabs in F# code; args; timeouts), exit status,
  PNG decoding, and refusal of a socket in a directory others can read.

## Failures (kept)
- `idle-run1/`: the two MCP configurations failed in 0.2 s ("MCP config file not found"). The script used a
  relative output path after changing directory. The script was fixed and all four calls were rerun as
  `idle-run2/`. This was a harness bug, not a change after seeing results. In total this phase made 6 model calls
  instead of the planned 4; the 2 failed ones never reached the model.
- While the check was being written, the CLI returned exit code 1 instead of 2 when the socket guard refused. This
  was fixed before commit.

## Implementation: dependencies
- `docs/agentic-gaia/prototype/gaia` (bash script): needs bash, OpenBSD netcat (`nc` with `-U -N`; Ubuntu's
  default `netcat-openbsd`), base64, stat, tr, sed, grep, awk. No Python, no extra server process. Each call
  opens one connection, sends one JSON line, and half-closes; the bridge answers and closes. (The bridge handles
  one request at a time per connection; checked in `GaiaAgentBridge.fs` `handleConnection`.)
- Coverage: ping, status, eval (argument or stdin), advance, hierarchy, inspect, log, symbol, resolve_exception,
  undo/redo, reload_code, checkpoint/diff/assert, capture (engine readback to PNG; game view), frames (separate
  PNGs), restart (same safety checks as MCP: pid must be Nu.Gaia.dll bound to our socket), docs (grep over
  Nu.xml), raw (any op).
- Known differences from MCP (recorded in the protocol, not hidden): images are files the agent must `Read`
  (MCP sends them inline); no 1280 px downsizing; frames are separate files, not one contact sheet; docs search
  is plain grep (no scoring); no X-screenshot mode; replies are raw bridge JSON (MCP trims some fields). A bad
  number argument is reported but the request still goes out and fails at the bridge (exit 1).
- The Python MCP server, client and old results are unchanged.

## Still needed
1. **GPU gate:** wait until Nu-value's GPU trial ends. Their verdict was "scored rounds running", and their Xvfb
   :94 was up at 00:22 HKT.
2. Live validation of every `gaia` command against Gaia on the NVIDIA GPU, including a capture that matches MCP's
   capture of the same frame (protocol, "Validation").
3. Optional: one extra idle call, Bash without the usage text, to split the terminal cost exactly.
4. Task pilot (protocol condition 2): `bounce` and `rows`, 3 runs per arm, Opus 5.5. Each costly stage needs a
   fresh quota check (90% weekly at 00:26 HKT).
