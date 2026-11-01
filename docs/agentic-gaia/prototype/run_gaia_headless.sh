#!/usr/bin/env bash
# Start Gaia headless on Linux: private Xvfb display + Mesa lavapipe (software Vulkan 1.3) + agent bridge socket.
# Usage: run_gaia_headless.sh [PROJECT_DLL]   (default: Projects/Breakout ImSim, Debug build)
# Env:   GAIA_DISPLAY (default :91), NU_GAIA_AGENT_SOCKET (default $XDG_RUNTIME_DIR/nu-gaia-agent.sock),
#        GAIA_VULKAN=lavapipe|nvidia|default (default lavapipe; nvidia cannot present to Xvfb)
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
GAIA_BIN="$ROOT/Nu/Nu.Gaia/bin/Debug/net10.0"
PROJECT_DLL="${1:-$ROOT/Projects/Breakout ImSim/bin/Debug/net10.0/Breakout ImSim.dll}"
export DISPLAY="${GAIA_DISPLAY:-:91}"
export NU_GAIA_AGENT_SOCKET="${NU_GAIA_AGENT_SOCKET:-${XDG_RUNTIME_DIR:-/tmp}/nu-gaia-agent.sock}"
export PATH="$HOME/.dotnet:$PATH" DOTNET_CLI_TELEMETRY_OPTOUT=1
LOG_DIR="${GAIA_LOG_DIR:-${XDG_RUNTIME_DIR:-/tmp}/nu-gaia-logs}"
mkdir -p -m 700 "$LOG_DIR"

case "${GAIA_VULKAN:-lavapipe}" in
  lavapipe) export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/lvp_icd.x86_64.json
            export LP_NUM_THREADS="${LP_NUM_THREADS:-4}" ;;  # bound software-rasteriser threads (shared server)
  nvidia)   export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/nvidia_icd.json ;;
  *) ;;
esac

# 1. display: reuse if a server already owns it, else start a private Xvfb (never touch :0/:77/:78/:99)
n="${DISPLAY#:}"
if [ ! -S "/tmp/.X11-unix/X$n" ]; then
  nohup Xvfb "$DISPLAY" -screen 0 1920x1080x24 -nolisten tcp </dev/null >"$LOG_DIR/xvfb.log" 2>&1 &
  for _ in $(seq 50); do [ -S "/tmp/.X11-unix/X$n" ] && break; sleep 0.1; done
fi

# 2. point Gaia at the project (Gaia reads Nu.Gaia.State.txt from its own directory)
[ -f "$PROJECT_DLL" ] || { echo "missing project dll: $PROJECT_DLL (build Nu.Gaia first)" >&2; exit 1; }
cat >"$GAIA_BIN/Nu.Gaia.State.txt" <<EOF
[[ProjectDllPath "$PROJECT_DLL"]
 [ProjectEditModeOpt None]
 [ProjectFreshlyLoaded True]
 [ProjectImperativeExecution False]
 [DesiredEye2dCenter [0 0]]
 [DesiredEye3dCenter [0 0 2]]
 [DesiredEye3dRotation [0 0 0 1]]
 [MasterSoundVolume 0]
 [MasterSongVolume 0]
 [Snaps2dSelected True]
 [Snaps2d [8 5 0.1]]
 [Snaps3d [0.1 5 0.1]]
 [CreationElevation 0]
 [CreationDistance 2]
 [AlternativeEyeTravelInput False]
 [OverlayMode False]]
EOF

# 3. launch Gaia (niced; shared server) and wait for the bridge socket
rm -f "$NU_GAIA_AGENT_SOCKET"
cd "$GAIA_BIN"
nohup nice -n 5 dotnet Nu.Gaia.dll </dev/null >"$LOG_DIR/gaia.log" 2>&1 &
echo $! >"$LOG_DIR/gaia.pid"
printf "%s\n" "$PROJECT_DLL" >"$LOG_DIR/gaia.project"  # lets gaia_restart relaunch the same project
for _ in $(seq 240); do [ -S "$NU_GAIA_AGENT_SOCKET" ] && break; sleep 0.5; done
if [ -S "$NU_GAIA_AGENT_SOCKET" ]; then
  echo "Gaia pid $(cat "$LOG_DIR/gaia.pid") on $DISPLAY; bridge $NU_GAIA_AGENT_SOCKET; log $LOG_DIR/gaia.log"
else
  echo "bridge socket did not appear; see $LOG_DIR/gaia.log" >&2; tail -20 "$LOG_DIR/gaia.log" >&2; exit 1
fi
