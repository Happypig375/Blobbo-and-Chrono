#!/usr/bin/env bash
# Start Gaia headless on Linux: private Xvfb display + Vulkan on the NVIDIA GPU + agent bridge socket.
# Usage: run_gaia_headless.sh [PROJECT_DLL]   (default: Projects/Breakout ImSim, Debug build)
# Env:   GAIA_DISPLAY (default :91), NU_GAIA_AGENT_SOCKET (default $XDG_RUNTIME_DIR/nu-gaia-agent.sock),
#        GAIA_LOG_DIR (default $XDG_RUNTIME_DIR/nu-gaia-logs),
#        GAIA_VULKAN=nvidia|lavapipe (default nvidia: the server's GPU; see docs/agentic-gaia/nvidia-vulkan-xvfb.md).
#          lavapipe (Mesa software Vulkan) is ONLY for reproducing a run that was recorded with software rendering;
#          say so in that run's protocol. There is no automatic fallback: no GPU, or a busy GPU, stops the launch.
#        GAIA_GPU_SHARE=1 starts even if something else is using the GPU (only after checking with its owner),
#        GAIA_EDIT_MODE (optional plugin edit mode, e.g. Gameplay)
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

VULKAN="${GAIA_VULKAN:-nvidia}"
case "$VULKAN" in
  nvidia)   ICD=/usr/share/vulkan/icd.d/nvidia_icd.json; DEVICE_RE='Selected Vulkan Device NVIDIA' ;;
  lavapipe) ICD=/usr/share/vulkan/icd.d/lvp_icd.x86_64.json; DEVICE_RE='Selected Vulkan Device llvmpipe'
            export LP_NUM_THREADS="${LP_NUM_THREADS:-4}"  # bound software-rasteriser threads (shared server)
            echo "note: GAIA_VULKAN=lavapipe = software rendering, only for reproducing a recorded software-rendered run;" \
                 "declare it in that run's protocol" >&2 ;;
  *) die "GAIA_VULKAN must be nvidia (default) or lavapipe, not '$VULKAN'" ;;
esac
[ -f "$ICD" ] || die "Vulkan driver file $ICD is missing"
export VK_ICD_FILENAMES="$ICD" VK_DRIVER_FILES="$ICD"  # older / newer Vulkan loaders: use this driver only
unset LIBGL_ALWAYS_SOFTWARE GALLIUM_DRIVER MESA_VK_DEVICE_SELECT

# GPU check (nvidia only; shared with labmates): the GPU must be visible and idle. Busy = any process on it other
# than the system's Xorg, or more than GAIA_GPU_MAX_USED_MB (512) memory in use or GAIA_GPU_MAX_UTIL (20) % busy.
gpu_check() {
  command -v nvidia-smi >/dev/null || die "no NVIDIA GPU: nvidia-smi is not installed. Not falling back to software" \
    "rendering; GAIA_VULKAN=lavapipe is only for reproducing a recorded software-rendered run."
  local table="" query="" prev="${1:-}" i
  for i in $(seq 30); do
    table="$(timeout 20 nvidia-smi 2>&1)" && query="$(timeout 20 nvidia-smi \
      --query-gpu=name,memory.used,utilization.gpu --format=csv,noheader,nounits 2>&1)" \
      || die "no usable NVIDIA GPU: nvidia-smi failed ($(printf '%s' "$table$query" | head -c 200)). Not falling back" \
        "to software rendering; GAIA_VULKAN=lavapipe is only for reproducing a recorded software-rendered run."
    # our own previous Gaia (just stopped, e.g. by gaia_restart) can stay on the GPU briefly as a zombie: wait for it
    [ -n "$prev" ] && awk -v p="$prev" '$1 == "|" && $5 == p {f = 1} END {exit !f}' <<<"$table" || break
    sleep 1
  done
  local name used util others
  IFS=, read -r name used util <<<"$(head -1 <<<"$query")"
  used="${used// /}"; util="${util// /}"
  [[ "$used" =~ ^[0-9]+$ && "$util" =~ ^[0-9]+$ ]] || die "cannot read GPU state from nvidia-smi: $query"
  # process rows look like: |    0   N/A  N/A    4325      G   /usr/lib/xorg/Xorg     4MiB |
  others="$(awk '/Processes:/ {p = 1} p && $1 == "|" && $2 ~ /^[0-9]+$/ && $5 ~ /^[0-9]+$/ && $7 !~ /(^|\/)Xorg$/ \
    {print "pid " $5 " (" $7 ", " $(NF-1) ")"}' <<<"$table" | paste -sd ';' -)"
  # utilisation is a short moving average: if it alone is high, confirm over 5 s before calling the GPU busy
  local max_util="${GAIA_GPU_MAX_UTIL:-20}"
  for i in 1 2 3 4 5; do
    { [ -z "$others" ] && [ "$util" -gt "$max_util" ]; } || break
    sleep 1
    util="$(timeout 20 nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits | head -1 | tr -d ' ')"
    [[ "$util" =~ ^[0-9]+$ ]] || die "cannot read GPU utilisation from nvidia-smi"
  done
  if [ -n "$others" ] || [ "$used" -gt "${GAIA_GPU_MAX_USED_MB:-512}" ] || [ "$util" -gt "$max_util" ]; then
    local what="GPU${name:+ ($name)} is busy: ${used} MiB used, ${util}% busy${others:+; in use by: $others}"
    if [ "${GAIA_GPU_SHARE:-0}" = 1 ]; then
      echo "note: $what; starting anyway because GAIA_GPU_SHARE=1" >&2
    else
      die "$what. It is shared with labmates: wait until it is free, or set GAIA_GPU_SHARE=1 after checking with" \
        "whoever uses it. Not falling back to software rendering."
    fi
  fi
}

# 0. one instance per log dir: refuse if the recorded Gaia is still alive
if [ -f "$LOG_DIR/gaia.pid" ]; then
  old="$(cat "$LOG_DIR/gaia.pid")"
  if kill -0 "$old" 2>/dev/null && tr '\0' ' ' <"/proc/$old/cmdline" 2>/dev/null | grep -q 'Nu.Gaia.dll'; then
    die "Gaia pid $old from this launcher is still running; stop it (or use gaia_restart) first"
  fi
fi

[ "$VULKAN" = nvidia ] && gpu_check "$(cat "$LOG_DIR/gaia.pid" 2>/dev/null || true)"

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
[ -S "$NU_GAIA_AGENT_SOCKET" ] || { tail -20 "$LOG_DIR/gaia.log" >&2; die "bridge socket did not appear"; }
# 4. no silent fallback: Gaia must have picked the requested device (the line is logged before the bridge opens)
device="$(grep -m1 'Selected Vulkan Device' "$LOG_DIR/gaia.log" | sed 's/.*Selected Vulkan Device //')"
if ! grep -q "$DEVICE_RE" "$LOG_DIR/gaia.log"; then
  kill "$pid" 2>/dev/null || true
  die "Gaia selected '${device:-no Vulkan device}' instead of the $VULKAN driver; stopped it (log: $LOG_DIR/gaia.log)"
fi
echo "Gaia pid $pid on $DISPLAY (XAUTHORITY=$XAUTHORITY); Vulkan device $device; bridge $NU_GAIA_AGENT_SOCKET; log $LOG_DIR/gaia.log"
