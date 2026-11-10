#!/usr/bin/env bash
# CPU-only check of the `gaia` terminal CLI against a stand-in bridge (netcat listening on a Unix socket).
# Checks the exact request lines the CLI sends (JSON escaping, args, timeouts), reply handling, exit status,
# PNG decoding and the socket ownership/permission guard. No Gaia, no GPU, no Python.
set -uo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
T="$(mktemp -d "${XDG_RUNTIME_DIR:?}/gaia-cli-check.XXXXXX")"; chmod 700 "$T"
trap 'rm -rf "${T:?}"' EXIT
export NU_GAIA_AGENT_SOCKET="$T/agent.sock" NU_GAIA_SHOT_DIR="$T/shots"
pass=0; fail=0
check() { if [ "$2" = "$3" ]; then pass=$((pass+1)); else fail=$((fail+1)); printf 'FAIL %s\n  want: %s\n  got:  %s\n' "$1" "$3" "$2"; fi; }

# serve REPLY: answer one connection with REPLY, record the request line in $T/req
serve() { rm -f "$NU_GAIA_AGENT_SOCKET"; { printf '%s\n' "$1"; } | nc -l -N -U "$NU_GAIA_AGENT_SOCKET" >"$T/req" & SRV=$!
  for _ in $(seq 50); do [ -S "$NU_GAIA_AGENT_SOCKET" ] && break; sleep 0.05; done; }
run() { G_OUT="$("$HERE/gaia" "$@" 2>"$T/err")"; G_RC=$?; wait "$SRV" 2>/dev/null; G_REQ="$(cat "$T/req")"; }

serve '{"id":"1","ok":true,"frame":7}'; run ping
check "ping request" "$G_REQ" '{"id":"1","op":"ping","args":{}}'
check "ping output" "$G_OUT" '{"id":"1","ok":true,"frame":7}'; check "ping rc" "$G_RC" 0

code=$'let s = "a\\b \\"q\\""\n\tprintfn "%s" s'
serve '{"id":"1","ok":true,"kind":"ok","output":"x"}'; run eval "$code" 30
check "eval escaping" "$G_REQ" '{"id":"1","op":"eval","args":{"code":"let s = \"a\\b \\\"q\\\"\"\n\tprintfn \"%s\" s"},"timeout_s":30}'
check "eval rc ok" "$G_RC" 0

serve '{"id":"1","ok":true,"kind":"compile_error","diagnostics":["e"]}'; run eval - <<<'1 +'
check "eval from stdin" "$G_REQ" '{"id":"1","op":"eval","args":{"code":"1 +"},"timeout_s":120}'
check "compile error rc" "$G_RC" 1

serve '{"id":"1","ok":true,"passed":false,"failedAtFrame":3}'; run assert 'true' 60 'Simulants.Ball.Position' noadvance
check "assert request" "$G_REQ" '{"id":"1","op":"assert","args":{"predicate":"true","frames":60,"probe":"Simulants.Ball.Position","advance":false},"timeout_s":62}'
check "assert failed rc" "$G_RC" 1

serve '{"id":"1","ok":true}'; run checkpoint before restore
check "checkpoint request" "$G_REQ" '{"id":"1","op":"checkpoint","args":{"name":"before","restore":true,"delete":false}}'

serve '{"id":"1","ok":false,"error":"busy"}'; run advance 600
check "advance request" "$G_REQ" '{"id":"1","op":"advance","args":{"frames":600},"timeout_s":80}'
check "error reply rc" "$G_RC" 1

png="$(printf 'not-really-a-png\001\002' | base64 -w0)"
serve "{\"id\":\"1\",\"ok\":true,\"png\":\"$png\",\"frame\":9}"; run capture "$T/shots/a.png"
check "capture request" "$G_REQ" '{"id":"1","op":"capture","args":{"settle":2}}'
check "capture path" "$G_OUT" "$T/shots/a.png"
check "capture bytes" "$(base64 -w0 <"$T/shots/a.png" 2>/dev/null)" "$png"

serve '{"id":"1","ok":true}'; run raw view_mode '{"mode":"capture"}'
check "raw request" "$G_REQ" '{"id":"1","op":"view_mode","args":{"mode":"capture"}}'

# guard: a group-readable socket directory is refused before anything is sent
mkdir -m 755 "$T/open"; NU_GAIA_AGENT_SOCKET="$T/open/agent.sock" "$HERE/gaia" ping >/dev/null 2>"$T/err"; rc=$?
check "open directory refused" "$rc" 2
check "open directory message" "$(grep -c 'must be mode 0700\|does not exist' "$T/err")" 1

echo "gaia CLI check: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
