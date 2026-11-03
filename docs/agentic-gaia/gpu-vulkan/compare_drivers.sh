#!/usr/bin/env bash
# Launch Gaia (Breakout ImSim) on each Vulkan driver in turn, compare readback vs X grab, time a short session.
# One Gaia at a time; all on our own Xvfb :92. Usage: [PROJECT=DLL] [GAIA_EDIT_MODE=M] compare_drivers.sh OUT_DIR DRIVER...
set -uo pipefail
OUT="$1"; shift
HERE="$(dirname "$0")"
L="$HERE/../prototype/run_gaia_headless.sh"
export GAIA_DISPLAY=:92 GAIA_LOG_DIR=$XDG_RUNTIME_DIR/gaia-nv-logs NU_GAIA_AGENT_SOCKET=$XDG_RUNTIME_DIR/gaia-nv/agent.sock
export DISPLAY=:92 XAUTHORITY=$GAIA_LOG_DIR/xauth
stop_gaia() {
  local p; p="$(cat "$GAIA_LOG_DIR/gaia.pid" 2>/dev/null)" || return 0
  kill "$p" 2>/dev/null && { tail --pid="$p" -f /dev/null & w=$!; sleep 15; kill $w 2>/dev/null; kill -9 "$p" 2>/dev/null; }
  rm -f "$NU_GAIA_AGENT_SOCKET"
}
i=0
for d in "$@"; do
  i=$((i+1)); tag="$i-$d"
  stop_gaia
  t0=$(date +%s.%N)
  GAIA_VULKAN="$d" "$L" ${PROJECT:+"$PROJECT"} >"$OUT/launch-$tag.txt" 2>&1 || { echo "{\"label\":\"$tag\",\"launch\":\"failed\"}"; cat "$OUT/launch-$tag.txt"; continue; }
  t1=$(date +%s.%N)
  pid="$(cat "$GAIA_LOG_DIR/gaia.pid")"
  grep -m1 "Selected Vulkan Device" "$GAIA_LOG_DIR/gaia.log" | sed "s/^/[$tag] /"
  echo "[$tag] launch_to_bridge_s $(echo "$t1 - $t0" | bc)"
  nvidia-smi --query-compute-apps=pid,used_memory --format=csv,noheader | sed "s/^/[$tag] gpu-proc /"
  python3 "$HERE/compare_capture.py" "$OUT/shot-$tag" | sed "s/^/[$tag] compare /"
  python3 "$HERE/time_session.py" "$pid" "$tag" | sed "s/^/[$tag] time /"
  cp "$GAIA_LOG_DIR/gaia.log" "$OUT/gaia-$tag.log"
done
stop_gaia
