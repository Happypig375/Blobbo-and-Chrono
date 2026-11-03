#!/usr/bin/env python3
"""Short scripted Gaia session for timing one Vulkan driver against another.

Usage: time_session.py GAIA_PID LABEL   (needs NU_GAIA_AGENT_SOCKET; DISPLAY/XAUTHORITY for the X grab)
Steps (each timed): render 600 editor frames without advancing game time; advance the game 600 frames
(in 6 requests of 100, the bridge maximum per request is 600); 20 renderer readbacks; 20 X screenshots;
one F# eval. Also reports the Gaia process's CPU seconds over the whole session. Prints one JSON line.
"""
import json
import os
import sys
import time

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "../prototype"))
from gaia_bridge import GaiaBridge, screenshot  # noqa: E402

TICK = os.sysconf("SC_CLK_TCK")


def cpu_seconds(pid):
    fields = open(f"/proc/{pid}/stat").read().rsplit(")", 1)[1].split()
    return (int(fields[11]) + int(fields[12])) / TICK  # utime + stime


def timed(fn):
    t0 = time.time()
    fn()
    return round(time.time() - t0, 3)


def ok(reply):
    if not reply.get("ok"):
        raise RuntimeError(reply)
    return reply


def main(pid, label):
    b = GaiaBridge()
    ok(b.request("wait_frames", frames=5))
    cpu0, wall0 = cpu_seconds(pid), time.time()
    out = {"label": label}
    out["editor_600_frames_s"] = timed(lambda: ok(b.request("wait_frames", frames=600)))
    out["advance_600_frames_s"] = timed(lambda: [ok(b.request("advance", frames=100)) for _ in range(6)])
    out["capture_x20_s"] = timed(lambda: [ok(b.request("capture", settle=2)) for _ in range(20)])
    status = ok(b.request("status"))
    out["x11_grab_x20_s"] = timed(lambda: [screenshot(window=status.get("window")) for _ in range(20)])
    out["eval_s"] = timed(lambda: ok(b.request("eval", code="1 + 1")))
    wall = time.time() - wall0
    out["total_s"] = round(wall, 2)
    out["gaia_cpu_cores_avg"] = round((cpu_seconds(pid) - cpu0) / wall, 2)
    out["editor_fps"] = round(600 / out["editor_600_frames_s"], 1)
    print(json.dumps(out))


if __name__ == "__main__":
    main(int(sys.argv[1]), sys.argv[2])
