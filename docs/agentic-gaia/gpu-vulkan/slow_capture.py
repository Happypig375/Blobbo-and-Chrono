#!/usr/bin/env python3
"""For a very slow renderer: wait until the editor thread is responsive, then ask for one readback (settle=0)
with a long timeout, and grab the X window too. Usage: slow_capture.py OUT_PREFIX"""
import base64, io, json, os, sys, time
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "../prototype"))
from gaia_bridge import GaiaBridge, screenshot
from PIL import Image
b = GaiaBridge(timeout=900)
t0 = time.time()
while True:
    r = b.request("ping")
    if not r.get("unresponsive"):
        break
    time.sleep(1)
status = b.request("status", timeout_s=900)
r = b.request("capture", timeout_s=900, settle=0)
print(json.dumps({"ok": r.get("ok"), "error": r.get("error"), "frame": r.get("frame"), "s": round(time.time() - t0, 1)}))
if r.get("ok"):
    Image.open(io.BytesIO(base64.b64decode(r["png"]))).convert("RGB").save(sys.argv[1] + "-engine.png")
    screenshot(window=status.get("window")).convert("RGB").save(sys.argv[1] + "-x11.png")
