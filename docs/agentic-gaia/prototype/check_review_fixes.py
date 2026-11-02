#!/usr/bin/env python3
"""Checks for the round-3 code-review fixes, against a running headless Gaia (run_gaia_headless.sh).

1. undo, redo, reload_code and checkpoint restore are refused while an assert holds time control;
2. a probe ending in a '// comment' compiles;
3. a long probe result is truncated;
4. the MCP server sends instructions, naming gaia_assert only when that tool is enabled.
Usage: check_review_fixes.py
"""
import json
import os
import subprocess
import sys
import threading
import time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from gaia_bridge import GaiaBridge  # noqa: E402

results = []


def check(name, ok, detail=""):
    results.append(ok)
    print(("PASS " if ok else "FAIL ") + name + (f"  [{detail}]" if detail else ""))


main = GaiaBridge()
side = GaiaBridge()

# 1. a long assert holds time control; other world-swapping ops must be refused meanwhile
main.request("checkpoint", name="review-fix")
long_assert = {}
worker = threading.Thread(target=lambda: long_assert.update(main.request("assert", timeout_s=300, predicate="true", frames=240)))
worker.start()
time.sleep(1.0)
for op, args in [("undo", {}), ("redo", {}), ("reload_code", {}), ("checkpoint", {"name": "review-fix", "restore": "true"}),
                 ("advance", {"frames": 1})]:
    reply = side.request(op, **args)
    check(f"{op} refused while an assert runs", reply.get("ok") is False and "in flight" in reply.get("error", ""), reply.get("error", "")[:80])
worker.join()
check("the long assert still passes", long_assert.get("passed") is True, f"checkedFrames={long_assert.get('checkedFrames')}")
reply = side.request("checkpoint", name="review-fix", restore="true")
check("restore works again afterwards", reply.get("ok") is True and reply.get("restored") == "review-fix")
side.request("checkpoint", name="review-fix", delete="true")

# 2. probe with a trailing line comment
reply = main.request("assert", predicate="true", frames=1, probe="1 + 1 // a comment")
check("probe with trailing comment compiles", reply.get("passed") is True and "2" in str(reply.get("probe")), str(reply.get("probe"))[:40])

# 3. long probe output is truncated
reply = main.request("assert", predicate="true", frames=1, probe='String.replicate 10000 "x"')
probe = str(reply.get("probe"))
check("long probe truncated", len(probe) < 4200 and "truncated" in probe, f"{len(probe)} characters")

# 4. server instructions per tool set
for level, expect in [("frames", "gaia_assert"), ("eval", "gaia_advance")]:
    init = json.dumps({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}}) + "\n"
    out = subprocess.run([sys.executable, "-I", os.path.join(HERE, "gaia_mcp.py")], input=init, capture_output=True, text=True,
                         timeout=20, env=dict(os.environ, NU_GAIA_TOOLSET=level)).stdout
    text = json.loads(out.splitlines()[0])["result"].get("instructions", "")
    names_hidden = level == "eval" and "gaia_assert" in text
    check(f"instructions for tool set '{level}' mention {expect} only where available", expect in text and not names_hidden)

print(f"\n{sum(results)}/{len(results)} checks passed")
sys.exit(0 if all(results) else 1)
