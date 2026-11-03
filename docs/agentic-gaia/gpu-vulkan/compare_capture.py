#!/usr/bin/env python3
"""Compare Gaia's renderer readback (bridge op "capture") with an X screenshot of the Xvfb window.

Usage: compare_capture.py OUT_PREFIX   (needs DISPLAY, XAUTHORITY, NU_GAIA_AGENT_SOCKET in the environment)
Writes OUT_PREFIX-engine.png and OUT_PREFIX-x11.png and prints simple statistics for each and their difference.
"""
import base64
import io
import json
import os
import sys
import time

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "../prototype"))
from gaia_bridge import GaiaBridge, screenshot  # noqa: E402
from PIL import Image, ImageChops, ImageStat  # noqa: E402


def stats(img):
    s = ImageStat.Stat(img)
    extrema = img.getextrema()
    colours = len(img.getcolors(1 << 24) or [])
    return {"size": img.size, "mean": [round(m, 1) for m in s.mean], "extrema": extrema, "colours": colours}


def main(prefix):
    b = GaiaBridge()
    status = b.request("wait_frames", frames=2)
    t0 = time.time()
    reply = b.request("capture", settle=2)
    t_engine = time.time() - t0
    if not reply.get("ok"):
        print("capture failed:", reply)
        return 1
    engine = Image.open(io.BytesIO(base64.b64decode(reply["png"]))).convert("RGB")
    t0 = time.time()
    x11 = screenshot(window=status.get("window")).convert("RGB")
    t_x11 = time.time() - t0
    engine.save(prefix + "-engine.png")
    x11.save(prefix + "-x11.png")
    out = {"window": status.get("window"), "engine": stats(engine), "x11": stats(x11),
           "engine_s": round(t_engine, 3), "x11_s": round(t_x11, 3)}
    if engine.size == x11.size:
        diff = ImageChops.difference(engine, x11)
        out["diff"] = {"bbox": diff.getbbox(), "mean": [round(m, 2) for m in ImageStat.Stat(diff).mean]}
    print(json.dumps(out))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
