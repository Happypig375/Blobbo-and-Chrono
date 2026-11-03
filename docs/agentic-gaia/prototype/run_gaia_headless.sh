#!/usr/bin/env bash
# Start Gaia headless on Linux: private Xvfb display + Mesa lavapipe (software Vulkan 1.3) + agent bridge socket.
# Usage: run_gaia_headless.sh [PROJECT_DLL]   (default: Projects/Breakout ImSim, Debug build)
# Env:   GAIA_DISPLAY (default :91), NU_GAIA_AGENT_SOCKET (default $XDG_RUNTIME_DIR/nu-gaia-agent.sock),
#        GAIA_LOG_DIR (default $XDG_RUNTIME_DIR/nu-gaia-logs),
#        GAIA_VULKAN=lavapipe|nvidia|default (default lavapipe; nvidia = the RTX GPU, presents to Xvfb too, see
#          docs/agentic-gaia/nvidia-vulkan-xvfb.md), GAIA_EDIT_MODE (optional plugin edit mode, e.g. Gameplay)
# Security: the bridge socket is code execution as this user, so everything lives under the private
# $XDG_RUNTIME_DIR (no /tmp fallback), and the X display requires an Xauthority cookie.
set -euo pipefail
die() { echo "run_gaia_headless: $*" >&2; exit 1; }
private_dir() {  # create if missing; then require owner = me, mode 0700, not a symlink
  [ -L "$1" ] && die "$1 is a symlink"
  mkdir -p -m 700 "$1"
  [ "$(stat -c '%u %a' "$1")" = "$(id -u) 700" ] || die "$1 must be owned by $(id -un) with mode 700"
}

[ -n "${XDG_RUNTIME_DIR:-}" ] || die "XDG_RUNTIME_DIR is not set; refusing to put the bridge socket in a shared directory"
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
GAIA_BIN="$ROOT/Nu/Nu.Gaia/bin/Debug/net10.0"
PROJECT_DLL="$(realpath -e "${1:-$ROOT/Projects/Breakout ImSim/bin/Debug/net10.0/Breakout ImSim.dll}" 2>/dev/null)" \
  || die "missing project dll: ${1:-Breakout ImSim} (build Nu.Gaia and the project first)"
EDIT_MODE="${GAIA_EDIT_MODE:-}"
case "$EDIT_MODE" in *[!A-Za-z0-9_]*) die "GAIA_EDIT_MODE may only contain letters, digits and _";; esac
if [ -n "$EDIT_MODE" ]; then EDIT_MODE_OPT="[Some \"$EDIT_MODE\"]"; else EDIT_MODE_OPT=None; fi
case "$PROJECT_DLL" in *'"'*|*']'*|*'['*) die "project path may not contain quotes or brackets (Gaia state file syntax)";; esac
export DISPLAY="${GAIA_DISPLAY:-:91}"
export NU_GAIA_AGENT_SOCKET="${NU_GAIA_AGENT_SOCKET:-$XDG_RUNTIME_DIR/nu-gaia-agent.sock}"
export PATH="$HOME/.dotnet:$PATH" DOTNET_CLI_TELEMETRY_OPTOUT=1
LOG_DIR="${GAIA_LOG_DIR:-$XDG_RUNTIME_DIR/nu-gaia-logs}"
private_dir "$LOG_DIR"
private_dir "$(dirname "$NU_GAIA_AGENT_SOCKET")"

case "${GAIA_VULKAN:-lavapipe}" in
  lavapipe) export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/lvp_icd.x86_64.json
            export VK_DRIVER_FILES=/usr/share/vulkan/icd.d/lvp_icd.x86_64.json
            export LP_NUM_THREADS="${LP_NUM_THREADS:-4}" ;;  # bound software-rasteriser threads (shared server)
  nvidia)   export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/nvidia_icd.json  # older Vulkan loaders
            export VK_DRIVER_FILES=/usr/share/vulkan/icd.d/nvidia_icd.json ;;  # newer loaders
  *) ;;
esac

# 0. one instance per log dir: refuse if the recorded Gaia is still alive
if [ -f "$LOG_DIR/gaia.pid" ]; then
  old="$(cat "$LOG_DIR/gaia.pid")"
  if kill -0 "$old" 2>/dev/null && tr '\0' ' ' <"/proc/$old/cmdline" 2>/dev/null | grep -q 'Nu.Gaia.dll'; then
    die "Gaia pid $old from this launcher is still running; stop it (or use gaia_restart) first"
  fi
fi

# 1. display: a private Xvfb with an Xauthority cookie (never touch :0/:77/:78/:99). Reuse only our own server.
n="${DISPLAY#:}"
export XAUTHORITY="$LOG_DIR/xauth"
xvfb_pid="$(pgrep -u "$(id -u)" -f "^Xvfb $DISPLAY " || true)"
if [ -S "/tmp/.X11-unix/X$n" ] && [ -z "$xvfb_pid" ]; then
  die "display $DISPLAY exists but is not an Xvfb owned by $(id -un) (another user's, or stale); pick another GAIA_DISPLAY"
fi
if [ -z "$xvfb_pid" ]; then
  rm -f "$XAUTHORITY"
  ( umask 077; xauth -q -f "$XAUTHORITY" add "$DISPLAY" . "$(mcookie)" 2>/dev/null )
  nohup Xvfb "$DISPLAY" -screen 0 1920x1080x24 -nolisten tcp -auth "$XAUTHORITY" </dev/null >"$LOG_DIR/xvfb.log" 2>&1 &
  for _ in $(seq 50); do [ -S "/tmp/.X11-unix/X$n" ] && break; sleep 0.1; done
fi
xdpyinfo >/dev/null 2>&1 || die "cannot connect to $DISPLAY with $XAUTHORITY; see $LOG_DIR/xvfb.log"

# 2. point Gaia at the project (Gaia reads Nu.Gaia.State.txt from its own bin directory, so this overwrites a human's
#    saved editor state for this build; keep one backup of a pre-existing state file)
STATE="$GAIA_BIN/Nu.Gaia.State.txt"
if [ -f "$STATE" ] && [ ! -f "$STATE.pre-agent" ] && ! grep -q 'MasterSoundVolume 0\]' "$STATE"; then
  cp -p "$STATE" "$STATE.pre-agent"
  echo "note: backed up your Gaia state to $STATE.pre-agent"
fi
cat >"$STATE" <<EOF
[[ProjectDllPath "$PROJECT_DLL"]
 [ProjectEditModeOpt $EDIT_MODE_OPT]
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

# 3. launch Gaia (niced; shared server) and wait for the bridge socket, failing fast if Gaia dies
cd "$GAIA_BIN"
nohup nice -n 5 dotnet Nu.Gaia.dll </dev/null >"$LOG_DIR/gaia.log" 2>&1 &
pid=$!
echo "$pid" >"$LOG_DIR/gaia.pid"
printf "%s\n" "$PROJECT_DLL" >"$LOG_DIR/gaia.project"  # lets gaia_restart relaunch the same project
for _ in $(seq 240); do
  if [ -S "$NU_GAIA_AGENT_SOCKET" ] && grep -q "Agent bridge listening" "$LOG_DIR/gaia.log" 2>/dev/null; then break; fi
  kill -0 "$pid" 2>/dev/null || { tail -20 "$LOG_DIR/gaia.log" >&2; die "Gaia exited during startup"; }
  sleep 0.5
done
if [ -S "$NU_GAIA_AGENT_SOCKET" ]; then
  echo "Gaia pid $pid on $DISPLAY (XAUTHORITY=$XAUTHORITY); bridge $NU_GAIA_AGENT_SOCKET; log $LOG_DIR/gaia.log"
else
  tail -20 "$LOG_DIR/gaia.log" >&2; die "bridge socket did not appear"
fi
