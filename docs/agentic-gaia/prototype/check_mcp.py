#!/usr/bin/env python3
"""Reproducible end-to-end check of the agentic-Gaia spike: MCP client -> gaia_mcp.py -> bridge -> live Gaia.

Prerequisite: run_gaia_headless.sh has started Gaia (Breakout ImSim project) on a private Xvfb display.
Usage: python3 -I check_mcp.py [OUT_DIR]   (writes before/after/frames PNGs and a JSON transcript to OUT_DIR)
The last block hangs the editor on purpose (non-terminating eval), checks liveness/withdrawal/fast-fail, then
restarts Gaia via gaia_restart. Set NU_GAIA_CHECK_HANG=0 to skip it.
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


def tool(tool_name, **args):
    return rpc("tools/call", {"name": tool_name, "arguments": args})["result"]


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
check("tools/list", len(tools) == 19, f"{len(tools)} tools")

status = json.loads(text_of(tool("gaia_status")))
check("status: project loaded, time halted", status["projectDllPath"].endswith("Breakout ImSim.dll") and not status["advancing"])
second = subprocess.run([os.path.join(HERE, "run_gaia_headless.sh")], capture_output=True, text=True, timeout=60)
check("launcher refuses a second instance while Gaia runs", second.returncode != 0 and "still running" in second.stderr, second.stderr.strip()[-120:])

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
win = json.loads(text_of(tool("gaia_status"))).get("window") or {}
want_h = round(win.get("height", 0) * 960 / max(1, win.get("width", 1)))
check("screenshot cropped to Gaia window (no root margin)", win and before.size == (960, want_h), f"window {win}, image {before.size}")
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

depth0 = json.loads(text_of(tool("gaia_status")))["undoDepth"]
r = json.loads(text_of(tool("gaia_symbol", name="World.createEntity")))
check("symbol adds no undo entry", json.loads(text_of(tool("gaia_status")))["undoDepth"] == depth0)
ov = (r.get("overloads") or [{}])[0]
check("gaia_symbol: F# signature + Nu.xml doc + declaration", "Group" in ov.get("signature", "") and "Create an entity" in ov.get("doc", "")
      and (r.get("declaration") or "").endswith(".fs:2398"), f"{r.get('declaration')}")

r = tool("nu_docs", query="createEntity", limit=5)
check("nu_docs finds createEntity", "createEntity" in text_of(r), text_of(r).splitlines()[0][:100] if text_of(r) else "")

# --- round 3: in-engine frame readback, checkpoints, diffs, asserts, tool sets
sys.path.insert(0, HERE)
from gaia_bridge import GaiaBridge, screenshot  # noqa: E402
from PIL import Image, ImageChops  # noqa: E402

side = GaiaBridge(path=env["NU_GAIA_AGENT_SOCKET"])
t0 = time.time()
cap = side.request("capture", settle=2)
engine = Image.open(io.BytesIO(base64.b64decode(cap.get("png", "")))).convert("RGB") if cap.get("ok") else None
seconds = time.time() - t0
x11 = screenshot(display=env["DISPLAY"], window=side.request("wait_frames", frames=2).get("window")).convert("RGB")
same = engine is not None and engine.size == x11.size and ImageChops.difference(engine, x11).getbbox() is None
check("engine frame readback equals the X11 window grab", same, f"{engine.size if engine else cap.get('error')} in {seconds:.2f}s")
if engine is not None:
    engine.save(os.path.join(OUT, "5-engine-capture.png"))
side.close()

r = tool("gaia_eval", code="open Breakout\nGame.SetGameState Gameplay world")
tool("gaia_advance", frames=150)
selected = json.loads(text_of(tool("gaia_status")))["selectedScreen"]
check("eval switches to the gameplay screen", not r["isError"] and selected == "Game/Gameplay", selected)
r = json.loads(text_of(tool("gaia_checkpoint", name="c1")))
check("checkpoint saved", r.get("checkpoint") == "c1" and r.get("simulants", 0) > 20, f"{r.get('simulants')} simulants")
tool("gaia_advance", frames=30)
r = json.loads(text_of(tool("gaia_diff", **{"from": "c1"})))
moved = [c for c in r.get("changed", []) if c["path"].endswith("/Ball") and c["property"] == "Position"]
check("diff after 30 frames shows the ball's position change", moved and moved[0]["before"] != moved[0]["after"], json.dumps(moved)[:160])
ball = '(Simulants.Gameplay / "Scene" / "Ball")'
r = tool("gaia_assert", predicate=f"{ball}.GetExists world")
check("assert passes on one frame", not r["isError"] and json.loads(text_of(r)).get("passed") is True)
t0 = json.loads(text_of(tool("gaia_status")))["updateTime"]
r = json.loads(text_of(tool("gaia_assert", predicate=f"({ball}.GetPosition world).Y > -1000.0f", frames=120, probe=f"{ball}.GetPosition world")))
after = json.loads(text_of(tool("gaia_status")))
check("assert over 120 frames advances time, passes, then halts", r.get("passed") is True and r.get("checkedFrames") == 120
      and after["updateTime"] - t0 >= 110 and not after["advancing"], f"updateTime {t0} -> {after['updateTime']}, probe {r.get('probe')}")
r = json.loads(text_of(tool("gaia_assert", predicate=f"({ball}.GetPosition world).Y > 10000.0f", frames=60, probe=f"{ball}.GetPosition world")))
check("failing assert reports frame 1 and the probe, and leaves time halted", r.get("passed") is False and r.get("failedAtFrame") == 1
      and r.get("probe") and not json.loads(text_of(tool("gaia_status")))["advancing"], json.dumps(r)[:160])
r = tool("gaia_assert", predicate="world.NoSuchMember")
check("assert compile error -> isError + diagnostics", r["isError"] and json.loads(text_of(r)).get("kind") == "compile_error")
tool("gaia_checkpoint", name="c1", restore=True)
r = json.loads(text_of(tool("gaia_diff", **{"from": "c1"})))
check("restore brings the world back to the checkpoint (empty diff)", r.get("changedTotal") == 0 and not r.get("added") and not r.get("removed"))
r = json.loads(text_of(tool("gaia_checkpoint", name="c1", delete=True)))
check("checkpoint deleted", r.get("deleted") is True and r.get("checkpoints") == [])

limited = subprocess.Popen([sys.executable, "-I", os.path.join(HERE, "gaia_mcp.py")], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                           env=dict(env, NU_GAIA_TOOLSET="eval"), text=True)
limited.stdin.write(json.dumps({"jsonrpc": "2.0", "id": 1, "method": "tools/list"}) + "\n"
                    + json.dumps({"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "gaia_screenshot", "arguments": {}}}) + "\n")
limited.stdin.close()
listed = json.loads(limited.stdout.readline())["result"]["tools"]
refused = json.loads(limited.stdout.readline())["result"]
limited.wait(timeout=10)
check("tool set 'eval' hides inspection and vision tools", len(listed) == 10 and refused["isError"], f"{len(listed)} tools")

# --- liveness: a non-terminating eval blocks the editor thread; the transport must say so and recover
if os.environ.get("NU_GAIA_CHECK_HANG", "1") != "0":
    sys.path.insert(0, HERE)
    from gaia_bridge import GaiaBridge

    r = tool("gaia_eval", code="while true do ()", timeout_s=3)
    body = json.loads(text_of(r))
    check("hung eval: times out after timeout_s, reported as started (not withdrawn)", r["isError"] and body.get("timedOut") and not body.get("withdrawn"), json.dumps(body)[:160])
    side = GaiaBridge(path=env["NU_GAIA_AGENT_SOCKET"])
    t0 = time.time()
    queued = side.request("status", timeout_s=2)
    check("request queued behind the hang is withdrawn on timeout (never runs late)", queued.get("timedOut") and queued.get("withdrawn") is True, f"{time.time() - t0:.1f}s")
    side.close()
    p = json.loads(text_of(tool("gaia_ping")))
    check("ping answers while blocked and names the running op", p.get("ok") and p.get("busyOp") == "eval", json.dumps(p)[:160])
    time.sleep(8)
    p = json.loads(text_of(tool("gaia_ping")))
    check("ping flags unresponsive after threshold", p.get("unresponsive") is True, f"msSinceFrame={p.get('msSinceFrame')}")
    t0 = time.time()
    r = tool("gaia_status")
    check("new requests fail fast while unresponsive", r["isError"] and "unresponsive" in text_of(r) and time.time() - t0 < 2, f"{time.time() - t0:.2f}s")
    t0 = time.time()
    r = tool("gaia_restart", confirm=True)
    check("gaia_restart recovers a fresh editor", not r["isError"] and json.loads(text_of(r))["status"]["undoDepth"] == 0, f"{time.time() - t0:.0f}s")
    r = json.loads(text_of(tool("gaia_symbol", name="World.createEntity")))
    check("first symbol call on a fresh session: docs resolved, no undo entry",
          "Create an entity" in ((r.get("overloads") or [{}])[0].get("doc") or "") and json.loads(text_of(tool("gaia_status")))["undoDepth"] == 0)
    r = tool("gaia_eval", code="1 + 1")
    check("eval after symbol-initialised session has no init noise", text_of(r).count("Referenced") == 0 and "val it: int = 2" in text_of(r), text_of(r)[:120])

server.stdin.close()
server.wait(timeout=10)
with open(os.path.join(OUT, "transcript.json"), "w") as f:
    json.dump(transcript, f, indent=1)
failed = [label for label, ok, _ in results if not ok]
print(f"\n{len(results) - len(failed)}/{len(results)} checks passed; artifacts in {OUT}")
sys.exit(1 if failed else 0)
