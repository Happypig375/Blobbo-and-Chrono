#!/usr/bin/env python3
"""Image parity between the Python-free `gaia` command and the MCP server (gaia_mcp.py) on a live Gaia.

With game time halted, capture the same frame through the CLI, then through MCP gaia_screenshot (max_width=0: no
downsizing), then through the CLI again (a determinism control), for the editor view and the game view. Pixels are
compared exactly. Also records the size MCP sends by default (max_width 1280) and the MCP frames contact sheet size.
PARITY_GAMEPLAY=1 first switches to the gameplay screen and also compares one gaia_assert result across the routes.
Usage: python3 -I check_capture_parity.py OUT_DIR   (Gaia must be running; writes PNGs and parity.json to OUT_DIR)
"""
import base64
import io
import json
import os
import subprocess
import sys

from PIL import Image, ImageChops

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.abspath(sys.argv[1])
os.makedirs(OUT, mode=0o700, exist_ok=True)
GAIA = os.path.join(HERE, "gaia")
env = dict(os.environ, NU_GAIA_SHOT_DIR=OUT, NU_GAIA_TOOLSET="all")
server = subprocess.Popen([sys.executable, "-I", os.path.join(HERE, "gaia_mcp.py")], stdin=subprocess.PIPE,
                          stdout=subprocess.PIPE, env=env, text=True)
next_id = [0]


def rpc(method, params=None):
    next_id[0] += 1
    server.stdin.write(json.dumps({"jsonrpc": "2.0", "id": next_id[0], "method": method, "params": params or {}}) + "\n")
    server.stdin.flush()
    return json.loads(server.stdout.readline())


def tool(name, **args):
    return rpc("tools/call", {"name": name, "arguments": args})["result"]


def mcp_image(result):
    data = next(c["data"] for c in result["content"] if c["type"] == "image")
    return Image.open(io.BytesIO(base64.b64decode(data))).convert("RGB")


def cli(*args):
    done = subprocess.run([GAIA, *args], capture_output=True, text=True, env=env, timeout=300)
    if done.returncode != 0:
        raise RuntimeError(f"gaia {args}: rc {done.returncode}: {done.stdout[-300:]} {done.stderr[-300:]}")
    return done.stdout


def compare(a, b):
    if a.size != b.size:
        return {"same_size": False, "sizes": [a.size, b.size]}
    diff = ImageChops.difference(a, b)
    bbox = diff.getbbox()
    differing = sum(1 for px in diff.convert("L").getdata() if px)
    return {"same_size": True, "identical": bbox is None, "differing_pixels": differing, "diff_bbox": bbox}


rpc("initialize", {"protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": {"name": "parity", "version": "0"}})
report = {"advancing_before": json.loads(cli("status")).get("advancing")}
if os.environ.get("PARITY_GAMEPLAY") == "1":  # go to the gameplay screen first (ball moving, bricks drawn)
    cli("eval", 'open Breakout\nGame.Set "GameState" Gameplay world')
    cli("advance", "60")
    report["screen"] = json.loads(cli("status")).get("selectedScreen")
    # behaviour parity: the same assert from the same checkpoint, through each route
    ball = '(Simulants.Gameplay / "Scene" / "Ball")'
    pred, probe = f"({ball}.GetPosition world).Y > -60.0f", f"{ball}.GetPosition world"
    cli("checkpoint", "parity")
    by_cli = json.loads(subprocess.run([GAIA, "assert", pred, "90", probe], capture_output=True, text=True, env=env).stdout)
    cli("checkpoint", "parity", "restore")
    by_mcp = json.loads(next(c["text"] for c in tool("gaia_assert", predicate=pred, frames=90, probe=probe)["content"] if c["type"] == "text"))
    cli("checkpoint", "parity", "restore")
    keys = ("passed", "checkedFrames", "failedAtFrame", "updateTimeAtFailure", "probe")
    report["assert_cli"] = {k: by_cli.get(k) for k in keys}
    report["assert_mcp"] = {k: by_mcp.get(k) for k in keys}
    report["assert_same"] = report["assert_cli"] == report["assert_mcp"]
cli("raw", "set_advancing", '{"on":"false"}')
for view in ("editor", "game"):
    a_path = os.path.join(OUT, f"{view}-cli-1.png")
    cli("capture", a_path, view)
    m = mcp_image(tool("gaia_screenshot", view=view, max_width=0))
    m.save(os.path.join(OUT, f"{view}-mcp.png"))
    c_path = os.path.join(OUT, f"{view}-cli-2.png")
    cli("capture", c_path, view)
    a, c = Image.open(a_path).convert("RGB"), Image.open(c_path).convert("RGB")
    default = mcp_image(tool("gaia_screenshot", view=view))
    report[view] = {"cli_size": a.size, "mcp_size": m.size, "cli_vs_mcp": compare(a, m), "cli_vs_cli": compare(a, c),
                    "mcp_default_size": default.size}
sheet = mcp_image(tool("gaia_frames", count=3, every=10))
report["mcp_frames_sheet_size"] = sheet.size
report["passed"] = all(report[v]["cli_vs_mcp"].get("identical") for v in ("editor", "game")) and report.get("assert_same", True)
json.dump(report, open(os.path.join(OUT, "parity.json"), "w"), indent=1)
print(json.dumps(report))
server.stdin.close()
server.wait(timeout=30)
sys.exit(0 if report["passed"] else 1)
