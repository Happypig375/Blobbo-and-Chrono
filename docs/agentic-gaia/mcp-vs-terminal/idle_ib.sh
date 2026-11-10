#!/usr/bin/env bash
# Protocol amendment 2, idle control IB: Bash tool without the gaia usage text; otherwise exactly IT in idle_context.sh.
# Usage: idle_ib.sh OUT_DIR
set -uo pipefail
OUT="${1:?usage: idle_ib.sh OUT_DIR}"; mkdir -p "$OUT"; OUT="$(cd "$OUT" && pwd)"
WORK="$(mktemp -d "${XDG_RUNTIME_DIR:?}/idle-ctx.XXXXXX")"; chmod 700 "$WORK"
trap 'rm -rf "${WORK:?}"' EXIT
claude --version >"$OUT/claude-version.txt"
FILES="Read,Edit,Write,Glob,Grep"
t0=$(date +%s.%N)
(cd "$WORK" && nice -n 5 timeout 300 claude -p "Reply with exactly the word OK and nothing else." --model claude-opus-5-5 \
  --output-format stream-json --verbose --max-turns 1 --permission-mode acceptEdits --setting-sources project,local \
  --no-session-persistence --strict-mcp-config --tools "$FILES,Bash" --allowedTools "$FILES,Bash(gaia:*)") >"$OUT/IB.jsonl" 2>"$OUT/IB.err"
rc=$?; t1=$(date +%s.%N)
echo "{\"label\":\"IB\",\"rc\":$rc,\"wall_s\":$(echo "$t1 - $t0" | bc)}" | tee -a "$OUT/runs.jsonl"
