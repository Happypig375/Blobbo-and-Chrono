#!/usr/bin/env bash
# GPU gate for this comparison (protocol amendment 2): the GPU is free enough for one Gaia when
#  - no Gaia (Nu.Gaia.dll) from any project is running and no other process is on the GPU (CPU builds do not count),
#  - GPU utilisation is below 10 % on two polls 2 s apart (the rule Nu-value adopted on 10 Nov), and
#  - the only GPU compute processes are the resident, idle buildinggpt-rag service (or none).
# Prints one line (OPEN ... or CLOSED: reason) and appends it to gpu-gate.log; exit 0 only when open.
set -uo pipefail
LOG="$(dirname "$0")/gpu-gate.log"
say() { echo "$(date -Is) $*" | tee -a "$LOG"; }
nu="$(pgrep -af '[N]u\.Gaia\.dll' | head -3 | tr '\n' ';')"  # Gaia anywhere; other Nu GPU users show up as GPU processes below
[ -z "$nu" ] || { say "CLOSED: Nu process running: $nu"; exit 1; }
u1="$(nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits | tr -d ' ')"; sleep 2
u2="$(nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits | tr -d ' ')"
apps="$(nvidia-smi --query-compute-apps=pid,process_name,used_memory --format=csv,noheader | tr '\n' ';')"
others="$(nvidia-smi --query-compute-apps=process_name --format=csv,noheader | grep -v '^/srv/buildinggpt-rag/venv/bin/python$' || true)"
{ [ "$u1" -lt 10 ] && [ "$u2" -lt 10 ]; } || { say "CLOSED: GPU busy ${u1}%/${u2}% apps=$apps"; exit 1; }
[ -z "$others" ] || { say "CLOSED: other GPU compute process: $others"; exit 1; }
say "OPEN util=${u1}%/${u2}% apps=$apps"
