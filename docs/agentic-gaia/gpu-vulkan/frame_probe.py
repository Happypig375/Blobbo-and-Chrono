#!/usr/bin/env python3
"""Poll Gaia's bridge 'ping' every STEP seconds for at most LIMIT seconds; print elapsed, frame and msSinceFrame.
Stops early once the editor has rendered TARGET frames. Usage: frame_probe.py LIMIT STEP TARGET"""
import json, os, sys, time
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "../prototype"))
from gaia_bridge import GaiaBridge
limit, step, target = (float(a) for a in sys.argv[1:4])
b = GaiaBridge(timeout=30)
t0 = time.time()
while time.time() - t0 < limit:
    r = b.request("ping")
    print(json.dumps({"t": round(time.time() - t0, 1), "frame": r.get("frame"), "msSinceFrame": r.get("msSinceFrame"),
                      "unresponsive": r.get("unresponsive")}), flush=True)
    if (r.get("frame") or 0) >= target:
        break
    time.sleep(step)
