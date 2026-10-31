#!/usr/bin/env python3
"""Reproducible end-to-end check of the agentic-Gaia spike: MCP client -> gaia_mcp.py -> bridge -> live Gaia.

Prerequisite: run_gaia_headless.sh has started Gaia (Breakout ImSim project) on a private Xvfb display.
Usage: python3 -I check_mcp.py [OUT_DIR]   (writes before/after/frames PNGs and a JSON transcript to OUT_DIR)
Exit status 0 only if every check passes.
"""
import base64
import io
import json
import os
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = sys.argv[1] if len(sys.argv) > 1 else os.path.join(os.environ.get("XDG_RUNTIME_DIR", "/tmp"), "nu-gaia-check")
os.makedirs(OUT, exist_ok=True)

env = dict(os.environ)
env.setdefault("NU_GAIA_AGENT_SOCKET", os.path.join(env.get("XDG_RUNTIME_DIR", "/tmp"), "nu-gaia-agent.sock"))
env.setdefault("DISPLAY", ":91")
env.setdefault("NU_XML_DOC", os.path.join(HERE, "../../../Nu/Nu/bin/Debug/net10.0/Nu.xml"))
env["NU_GAIA_SHOT_DIR"] = OUT

server = subprocess.Popen([sys.executable, "-I", os.path.join(HERE, "gaia_mcp.py")], stdin=subprocess.PIPE,
                          stdout=subprocess.PIPE, env=env, text=True)
transcript, results, next_id = [], [], [0]


def rpc(method, params=None):
    next_id[0] += 1
    msg = {"jsonrpc": "2.0", "id": next_id[0], "method": method, "params": params or {}}
    server.stdin.write(json.dumps(msg) + "\n")
    server.stdin.flush()
    t0 = time.time()
    reply = json.loads(server.stdout.readline())
    elapsed = time.time() - t0
    logged = json.loads(json.dumps(reply))
    for item in (logged.get("result") or {}).get("content", []) or []:
        if item.get("type") == "image":
            item["data"] = f"<{len(item['data'])} base64 chars>"
    transcript.append({"request": msg, "reply": logged, "seconds": round(elapsed, 3)})
    return reply


def tool(name, **args):
    return rpc("tools/call", {"name": name, "arguments": args})["result"]


def text_of(result):
    return "\n".join(c["text"] for c in result["content"] if c["type"] == "text")


def image_of(result, name):
    from PIL import Image

    data = next(c["data"] for c in result["content"] if c["type"] == "image")
    image = Image.open(io.BytesIO(base64.b64decode(data))).convert("RGB")
    image.save(os.path.join(OUT, name))
    return image


def check(label, ok, detail=""):
    results.append((label, bool(ok), detail))
    print(("PASS " if ok else "FAIL ") + label + (f"  [{detail}]" if detail else ""))


def diff_fraction(a, b):
    from PIL import ImageChops

    bbox_diff = ImageChops.difference(a, b).convert("L").point(lambda v: 255 if v > 24 else 0)
    hist = bbox_diff.histogram()
    return hist[255] / (a.width * a.height)


init = rpc("initialize", {"protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": {"name": "check", "version": "0"}})
check("initialize", init["result"]["serverInfo"]["name"] == "nu-gaia")
server.stdin.write(json.dumps({"jsonrpc": "2.0", "method": "notifications/initialized"}) + "\n")
server.stdin.flush()
tools = rpc("tools/list")["result"]["tools"]
check("tools/list", len(tools) >= 12, f"{len(tools)} tools")

status = json.loads(text_of(tool("gaia_status")))
check("status: project loaded, time halted", status["projectDllPath"].endswith("Breakout ImSim.dll") and not status["advancing"])

r = tool("gaia_eval", code="World.getScreens world |> Seq.length")
check("eval ok + value echoed (first eval; Linux CRLF fix)", not r["isError"] and "val it: int = 5" in text_of(r), text_of(r)[:120])

r = tool("gaia_eval", code='let (x : int) = "oops"')
diag = json.loads(text_of(r)).get("diagnostics", [{}])[0]
check("compile error -> isError + FS0001 with line/col", r["isError"] and diag.get("Code") == "FS0001" and diag.get("StartLine") == 1, json.dumps(diag)[:120])

r = tool("gaia_eval", code='(failwith "boom" : unit)')
check("runtime exception -> isError + exception text", r["isError"] and "boom" in text_of(r))

hierarchy = json.loads(text_of(tool("gaia_hierarchy")))
paths = [e["path"] for s in hierarchy for g in s["groups"] for e in g["entities"]]
check("hierarchy lists Title/Gui/Play", "Game/Title/Gui/Play" in paths, f"{len(paths)} entities")

props = json.loads(text_of(tool("gaia_inspect", path="Game/Title/Gui/Play")))["properties"]
names = {p["name"] for p in props}
check("inspect Play button has Text, Position", {"Text", "Position"} <= names, f"{len(props)} properties")

# --- design iteration with visual feedback: add a big coloured block in the selected group, look, undo, look
before = image_of(tool("gaia_screenshot", view="game", max_width=960), "1-before.png")
code = (
    'let e = World.createEntity<StaticSpriteDispatcher> None DefaultOverlay (Some [|"AgentBlock"|]) SelectedGroup world\n'
    "e.SetSize (v3 400.0f 300.0f 0.0f) world\n"
    "e.SetPosition (v3 0.0f 0.0f 0.0f) world\n"
    "e.SetColor (Color (1.0f, 0.2f, 0.6f, 1.0f)) world\n"
    "string e"
)
r = tool("gaia_eval", code=code)
check("eval creates entity", not r["isError"] and "AgentBlock" in text_of(r), text_of(r)[:160])
after = image_of(tool("gaia_screenshot", view="game", max_width=960), "2-after.png")
changed = diff_fraction(before, after)
check("screenshot shows the change", changed > 0.02, f"{changed:.1%} of pixels changed")

r = tool("gaia_undo")
check("undo reverts eval", json.loads(text_of(r)).get("undone") is True)
undone = image_of(tool("gaia_screenshot", view="game", max_width=960), "3-undone.png")
residual = diff_fraction(before, undone)
check("screenshot after undo matches before", residual < 0.005, f"{residual:.2%} differ")
r = tool("gaia_eval", code='(Nu.Entity "Game/Splash/SlideGroup/AgentBlock").GetExists world')
check("entity gone after undo", "false" in text_of(r))

# --- time control and motion
t0 = json.loads(text_of(tool("gaia_status")))["updateTime"]
r = json.loads(text_of(tool("gaia_advance", frames=30)))
check("advance 30 frames then halt", r["updateTime"] - t0 >= 25 and not r["advancing"], f"updateTime {t0} -> {r['updateTime']}")
r = tool("gaia_frames", count=4, every=15, view="game")
image_of(r, "4-frames.png")
check("frame sequence contact sheet", not r["isError"])

r = tool("nu_docs", query="createEntity", limit=5)
check("nu_docs finds createEntity", "createEntity" in text_of(r), text_of(r).splitlines()[0][:100] if text_of(r) else "")

server.stdin.close()
server.wait(timeout=10)
with open(os.path.join(OUT, "transcript.json"), "w") as f:
    json.dump(transcript, f, indent=1)
failed = [label for label, ok, _ in results if not ok]
print(f"\n{len(results) - len(failed)}/{len(results)} checks passed; artifacts in {OUT}")
sys.exit(1 if failed else 0)
