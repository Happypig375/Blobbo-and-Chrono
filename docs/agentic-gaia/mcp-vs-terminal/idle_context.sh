#!/usr/bin/env bash
# Condition 1 of protocol.md: context cost of the Gaia tools when connected but never used.
# Four one-turn `claude -p` calls (I0, IT, IM, IMF), run one after another. No Gaia, no GPU.
# Usage: idle_context.sh OUT_DIR
set -uo pipefail
OUT="${1:?usage: idle_context.sh OUT_DIR}"; mkdir -p "$OUT"; OUT="$(cd "$OUT" && pwd)"
REPO=/home/happypig375/workspace/nu-gaia-agentic
PROTO="$REPO/docs/agentic-gaia/prototype"
WORK="$(mktemp -d "${XDG_RUNTIME_DIR:?}/idle-ctx.XXXXXX")"; chmod 700 "$WORK"   # neutral, empty project dir
trap 'rm -rf "${WORK:?}"' EXIT
PROMPT="Reply with exactly the word OK and nothing else."
MODEL=claude-opus-5-5
mkdir -m 700 "$WORK/rt"
# MCP server config as the ablation harness writes it (bridge socket absent: Gaia is not running, never used)
cat >"$OUT/gaia.mcp.json" <<EOF
{"mcpServers": {"gaia": {"command": "$(command -v python3)", "args": ["-I", "$PROTO/gaia_mcp.py"],
  "env": {"XDG_RUNTIME_DIR": "$WORK/rt", "NU_GAIA_AGENT_SOCKET": "$WORK/rt/agent.sock", "HOME": "$HOME"}}}}
EOF
# Terminal arm: CLI usage text + the same advice as the MCP server instructions, tool names mapped to commands
{ echo "A command \`gaia\` (on PATH) talks to a live Gaia editor (Nu's game editor) running this project. Usage:"
  "$PROTO/gaia" --help
  echo
  echo "A live Gaia editor is running the project. Compiling is not checking: after changing the game, reload the code" \
       "(gaia reload_code), then check in the running game that the change does what was asked, with a test that can" \
       "fail, such as gaia assert over enough frames to see the behaviour. If the check fails, your idea of the cause may" \
       "be wrong; look again before changing more code."
} >"$OUT/terminal-usage.txt"
# Offline sizes: MCP tools/list reply and initialize reply (instructions)
printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18"}}' \
  '{"jsonrpc":"2.0","id":2,"method":"tools/list"}' \
  | XDG_RUNTIME_DIR="$WORK/rt" NU_GAIA_AGENT_SOCKET="$WORK/rt/agent.sock" timeout 30 python3 -I "$PROTO/gaia_mcp.py" >"$OUT/mcp-offline.jsonl" 2>"$OUT/mcp-offline.err"
claude --version >"$OUT/claude-version.txt"

BASE=(-p "$PROMPT" --model "$MODEL" --output-format stream-json --verbose --max-turns 1
      --permission-mode acceptEdits --setting-sources project,local --no-session-persistence --strict-mcp-config)
FILES="Read,Edit,Write,Glob,Grep"
one() { # LABEL ENV... -- ARGS...
  local label="$1"; shift; local envs=(); while [ "$1" != -- ]; do envs+=("$1"); shift; done; shift
  local t0 t1; t0=$(date +%s.%N)
  (cd "$WORK" && env "${envs[@]}" nice -n 5 timeout 300 claude "${BASE[@]}" "$@") >"$OUT/$label.jsonl" 2>"$OUT/$label.err"
  local rc=$?; t1=$(date +%s.%N)
  echo "{\"label\":\"$label\",\"rc\":$rc,\"wall_s\":$(echo "$t1 - $t0" | bc)}" >>"$OUT/runs.jsonl"
}
one I0  X=1 -- --tools "$FILES" --allowedTools "$FILES"
one IT  X=1 -- --tools "$FILES,Bash" --allowedTools "$FILES,Bash(gaia:*)" --append-system-prompt "$(cat "$OUT/terminal-usage.txt")"
one IM  X=1 -- --tools "$FILES,ToolSearch" --allowedTools "$FILES,mcp__gaia" --mcp-config "$OUT/gaia.mcp.json"
one IMF ENABLE_TOOL_SEARCH=false -- --tools "$FILES,ToolSearch" --allowedTools "$FILES,mcp__gaia" --mcp-config "$OUT/gaia.mcp.json"
cat "$OUT/runs.jsonl"
