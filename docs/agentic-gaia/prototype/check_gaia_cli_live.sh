#!/usr/bin/env bash
# Live check of the Python-free `gaia` command against a running Gaia (Breakout ImSim), mirroring check_mcp.py's
# coverage. Prerequisite: run_gaia_headless.sh has started Gaia. Usage: check_gaia_cli_live.sh OUT_DIR
# The launcher must be able to relaunch Gaia (on a shared GPU: GAIA_GPU_SHARE=1 in the environment).
# Prints PASS/FAIL per check and a summary line; exit 0 only if every check passes. `restart` runs last.
set -uo pipefail
OUT="${1:?usage: check_gaia_cli_live.sh OUT_DIR}"; mkdir -p -m 700 "$OUT"; OUT="$(cd "$OUT" && pwd)"
HERE="$(cd "$(dirname "$0")" && pwd)"
G="$HERE/gaia"
export NU_XML_DOC="${NU_XML_DOC:-$HERE/../../../Nu/Nu/bin/Debug/net10.0/Nu.xml}"
pass=0; fail=0
check() { # LABEL EXPECTED_RC CMD... ; also keeps stdout in $OUT/LABEL.out
  local label="$1" want="$2"; shift 2
  local t0 t1 rc; t0=$(date +%s.%N)
  "$@" >"$OUT/$label.out" 2>"$OUT/$label.err"; rc=$?
  t1=$(date +%s.%N)
  LAST="$(cat "$OUT/$label.out")"
  if [ "$rc" = "$want" ]; then pass=$((pass + 1)); r=PASS; else fail=$((fail + 1)); r=FAIL; fi
  printf '%s %s rc=%s (want %s) %.2fs %s\n' "$r" "$label" "$rc" "$want" "$(echo "$t1 - $t0" | bc)" "$(head -c 160 "$OUT/$label.out" | tr '\n' ' ')"
}
expect() { # LABEL CONDITION_TEXT TEST... (a content check on the last output)
  local label="$1" what="$2"; shift 2
  if "$@"; then pass=$((pass + 1)); echo "PASS $label ($what)"; else fail=$((fail + 1)); echo "FAIL $label ($what)"; fi
}
has() { [[ "$LAST" == *"$1"* ]]; }
png_ok() { [ "$(head -c 8 "$1" | od -An -tx1 | tr -d ' \n')" = 89504e470d0a1a0a ] && [ "$(stat -c %s "$1")" -gt 1000 ]; }

check ping 0 "$G" ping;                 expect ping-fields '"ok":true' has '"ok":true'
check status 0 "$G" status;             expect status-screen 'selectedScreen present' has 'selectedScreen'
check eval-ok 0 "$G" eval '1 + 2';      expect eval-ok-value 'val it: int = 3' has 'val it: int = 3'
check eval-quotes 0 "$G" eval 'sprintf "a\"b\\c\t%d" 1';  expect eval-quotes-value 'escaped string round trip' has 'a\u0022b\\c\t1'
check eval-stdin 0 bash -c "printf 'let x = 40\nx + 2\n' | '$G' eval -";  expect eval-stdin-value 'val it: int = 42' has 'val it: int = 42'
check eval-compile-error 1 "$G" eval 'this is not F#';     expect eval-compile-kind 'kind compile_error' has '"kind":"compile_error"'
check eval-runtime-error 1 "$G" eval 'failwith<int> "boom"';    expect eval-runtime-kind 'exception reported' has 'boom'
check hierarchy 0 "$G" hierarchy;       expect hierarchy-splash 'Splash screen listed' has 'Splash'
check inspect-screen 0 "$G" inspect
check eval-to-gameplay 0 "$G" eval $'open Breakout\nGame.Set "GameState" Gameplay world'
check advance 0 "$G" advance 60;        expect advance-gameplay 'Game/Gameplay selected' has 'Game/Gameplay'
check inspect-ball 0 "$G" inspect 'Game/Gameplay/Scene/Ball'; expect inspect-ball-velocity 'LinearVelocity listed' has 'LinearVelocity'
check checkpoint-save 0 "$G" checkpoint c1
check advance-30 0 "$G" advance 30
check diff 0 "$G" diff c1 20;           expect diff-changed 'Ball position changed' has 'Ball'
check assert-pass 0 "$G" assert '(Simulants.Gameplay / "Scene" / "Ball").GetExists world' 30 '(Simulants.Gameplay / "Scene" / "Ball").GetPosition world'
expect assert-pass-probe 'probe value returned' has '"probe"'
check assert-fail 1 "$G" assert '(Simulants.Gameplay / "Scene" / "Ball").GetPosition world = v3Zero' 5;  expect assert-fail-frame 'failedAtFrame reported' has 'failedAtFrame'
check assert-noadvance 0 "$G" assert 'true' 1 '' noadvance
check assert-compile-error 1 "$G" assert 'not a predicate at all' 1
check checkpoint-restore 0 "$G" checkpoint c1 restore
check checkpoint-delete 0 "$G" checkpoint c1 delete
check capture-editor 0 "$G" capture "$OUT/editor.png";   expect capture-editor-png 'valid PNG' png_ok "$OUT/editor.png"
check capture-game 0 "$G" capture "$OUT/game.png" game;  expect capture-game-png 'valid PNG' png_ok "$OUT/game.png"
check status-after-capture 0 "$G" status
check frames 0 "$G" frames 3 10 "$OUT/frames"
expect frames-files '3 valid PNGs' bash -c "for f in '$OUT'/frames/frame-0{0,1,2}.png; do [ \"\$(stat -c %s \"\$f\")\" -gt 1000 ] || exit 1; done"
check undo 0 "$G" undo
check redo 0 "$G" redo
check reload-code 0 "$G" reload_code;   expect reload-code-log 'Code updated.' has 'Code updated.'
check log 0 "$G" log 0;                 expect log-nonempty 'log text' has '"log"'
check symbol 0 "$G" symbol World.getEntities;  expect symbol-decl 'declaration returned' has 'declaration'
check docs 0 "$G" docs linear velocity; expect docs-hit 'LinearVelocity member found' has 'LinearVelocity'
check docs-none 1 "$G" docs zzqqxxnotaword
check raw-status 0 "$G" raw status '{}'
check unknown-op 2 "$G" frobnicate
check bad-number 1 "$G" advance abc
check restart 0 "$G" restart;           expect restart-new-pid 'stopped the old pid and relaunched' has '"stopped":'
check ping-after-restart 0 "$G" ping
echo "SUMMARY pass=$pass fail=$fail"
[ "$fail" -eq 0 ]
