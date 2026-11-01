#!/usr/bin/env python3
"""Minimal MCP server (stdio, JSON-RPC 2.0, standard library + Pillow) over Gaia's agent bridge.

Spike quality: implements initialize, tools/list, tools/call and ping. Register with e.g.
  claude mcp add gaia -- python3 /path/to/gaia_mcp.py
with NU_GAIA_AGENT_SOCKET and DISPLAY in the environment (see run_gaia_headless.sh).
"""
import base64
import io
import json
import os
import re
import signal
import subprocess
import sys
import time
import xml.etree.ElementTree as ET

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gaia_bridge import GaiaBridge, check_private, private_runtime_dir, screenshot  # noqa: E402

PROTOCOL_VERSION = "2025-06-18"
SHOT_DIR = os.environ.get("NU_GAIA_SHOT_DIR") or os.path.join(private_runtime_dir(), "nu-gaia-shots")
EVAL_TIMEOUT_S = 30.0  # default for gaia_eval: the MCP loop is serial, so a hang should surface quickly
NU_XML = os.environ.get("NU_XML_DOC")  # path to Nu.xml (generated next to Nu.dll)
LOG_DIR = os.environ.get("GAIA_LOG_DIR") or os.path.join(private_runtime_dir(), "nu-gaia-logs")
if not os.environ.get("XAUTHORITY") and os.path.exists(os.path.join(LOG_DIR, "xauth")):
    os.environ["XAUTHORITY"] = os.path.join(LOG_DIR, "xauth")  # cookie written by run_gaia_headless.sh
LAUNCHER = os.path.join(os.path.dirname(os.path.abspath(__file__)), "run_gaia_headless.sh")
# engine: read the rendered frame back from Vulkan (exact, no X); x11: grab the X display and crop to the window
CAPTURE = os.environ.get("NU_GAIA_CAPTURE", "engine")
# Tool sets for ablation runs (design.md section 7): each level adds one feedback channel to the previous one.
TOOLSET_LEVELS = {
    "eval": ["gaia_status", "gaia_eval", "gaia_ping", "gaia_resolve_exception", "gaia_restart", "gaia_undo",
             "gaia_redo", "gaia_advance", "gaia_reload_code", "gaia_log"],
    "inspect": ["gaia_hierarchy", "gaia_inspect", "gaia_symbol", "nu_docs", "gaia_checkpoint", "gaia_diff", "gaia_assert"],
    "screenshot": ["gaia_screenshot"],
    "frames": ["gaia_frames"],
}

TOOLS = [
    {
        "name": "gaia_status",
        "description": "Editor status: frame counter, game update time, whether time is advancing, project, selection, undo depth.",
        "inputSchema": {"type": "object", "properties": {}},
    },
    {
        "name": "gaia_eval",
        "description": (
            "Evaluate F# in Gaia's live interactive session on the editor thread (Prime, Nu, Nu.Gaia and the project "
            "are opened; open the project's own namespace yourself if needed, e.g. `open Breakout`). `world`, "
            "`SelectedScreen`, `SelectedGroup`, `SelectedEntity` are bound when mentioned. "
            "Each eval snapshots the world first, so gaia_undo reverts its world changes (not files or FSI definitions). "
            "Returns kind ok|compile_error|runtime_error, FSI output, and diagnostics with line/column. "
            "Keep snippets small; a non-terminating eval freezes the editor (then use gaia_ping to confirm and "
            "gaia_restart to recover). timeout_s (default 30) bounds the wait; a timed-out eval cannot be cancelled. "
            "If Gaia's exception dialog is open (status.pendingException), call gaia_resolve_exception first."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {"code": {"type": "string"}, "timeout_s": {"type": "number", "minimum": 1, "maximum": 600}},
            "required": ["code"],
        },
    },
    {
        "name": "gaia_ping",
        "description": (
            "Liveness check answered off the editor thread (works while it is blocked): last frame, ms since that "
            "frame, the op currently running and for how long, queue length, unresponsive flag."
        ),
        "inputSchema": {"type": "object", "properties": {}},
    },
    {
        "name": "gaia_resolve_exception",
        "description": (
            "Resolve Gaia's 'Unhandled Exception!' dialog (shown in status.pendingException after a frame threw): "
            "hard_rewind undoes the last edit; ignore keeps the world as-is. Most tools refuse while it is open."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {"choice": {"type": "string", "enum": ["hard_rewind", "ignore"]}},
            "required": ["choice"],
        },
    },
    {
        "name": "gaia_restart",
        "description": (
            "Kill and relaunch the headless Gaia this server started (same project). Use only when gaia_ping shows "
            "the editor unresponsive, e.g. after a non-terminating eval. Unsaved world state, undo history and FSI "
            "definitions are lost; project files on disk are untouched."
        ),
        "inputSchema": {"type": "object", "properties": {"confirm": {"type": "boolean", "const": True}}, "required": ["confirm"]},
    },
    {
        "name": "gaia_undo",
        "description": "Undo the last editor operation (including the world effects of the last eval).",
        "inputSchema": {"type": "object", "properties": {}},
    },
    {
        "name": "gaia_redo",
        "description": "Redo the last undone editor operation.",
        "inputSchema": {"type": "object", "properties": {}},
    },
    {
        "name": "gaia_advance",
        "description": "Run game time for N editor frames, then halt again (snapshotted; undoable).",
        "inputSchema": {"type": "object", "properties": {"frames": {"type": "integer", "minimum": 1, "maximum": 600}}, "required": ["frames"]},
    },
    {
        "name": "gaia_hierarchy",
        "description": "List screens, groups and entities (depth-first) with dispatcher type names.",
        "inputSchema": {"type": "object", "properties": {}},
    },
    {
        "name": "gaia_inspect",
        "description": "List a simulant's properties with types and values. Path: '' = game, 'Screen', 'Screen/Group', 'Screen/Group/Entity[/Child]'.",
        "inputSchema": {"type": "object", "properties": {"path": {"type": "string"}}, "required": ["path"]},
    },
    {
        "name": "gaia_screenshot",
        "description": (
            "Render 2 frames and capture the editor window as PNG. view=game hides editor chrome (Gaia capture mode) "
            "and restores it afterwards."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {
                "view": {"type": "string", "enum": ["editor", "game"]},
                "max_width": {"type": "integer", "minimum": 160, "maximum": 1920},
            },
        },
    },
    {
        "name": "gaia_frames",
        "description": (
            "Advance game time and capture a frame every `every` frames, `count` times; returns one contact-sheet PNG "
            "(left-to-right, top-to-bottom). Use to see motion/animation. Undo restores the pre-advance world."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {
                "count": {"type": "integer", "minimum": 2, "maximum": 16},
                "every": {"type": "integer", "minimum": 1, "maximum": 120},
                "view": {"type": "string", "enum": ["editor", "game"]},
            },
            "required": ["count", "every"],
        },
    },
    {
        "name": "gaia_reload_code",
        "description": (
            "Hot-reload the project's F# source files from disk into the live world (Gaia F9). Returns the build log "
            "('Code updated.' on success, 'Failed to compile' with errors otherwise). Afterwards both the old and new "
            "definitions exist in the session: if an extension member is then reported ambiguous (FS0816), use the "
            "generic property API instead, e.g. `Game.Set \"GameState\" Gameplay world` or `screen.Get<int> \"Score\" world`. "
            "Objects that already exist may keep old values; re-enter the screen or restore a checkpoint to see the change."
        ),
        "inputSchema": {"type": "object", "properties": {}},
    },
    {
        "name": "gaia_log",
        "description": "Editor log text after a given log sequence number (see gaia_status.logSeq).",
        "inputSchema": {"type": "object", "properties": {"since": {"type": "integer"}}},
    },
    {
        "name": "gaia_symbol",
        "description": (
            "Resolve a name (e.g. 'World.createEntity', 'Entity.SetColor', 'MyGame.MyDispatcher') in the live F# session, "
            "with the session's opens: F# signature for each overload, XML doc summary, and declaration file:line "
            "(read the source there when docs are thin)."
        ),
        "inputSchema": {"type": "object", "properties": {"name": {"type": "string"}}, "required": ["name"]},
    },
    {
        "name": "gaia_checkpoint",
        "description": (
            "Save a named checkpoint of the whole world (kept outside the undo stack; at most 16), restore one "
            "(restore=true; the restore itself is undoable), or delete one (delete=true). Use before risky edits, "
            "and with gaia_diff to see exactly what an edit or a run of game time changed."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {"name": {"type": "string", "maxLength": 64}, "restore": {"type": "boolean"}, "delete": {"type": "boolean"}},
            "required": ["name"],
        },
    },
    {
        "name": "gaia_diff",
        "description": (
            "Compare the current world with a checkpoint: simulants added and removed, and every changed property "
            "with its value before and after (symbolic text). `limit` caps the changed list (default 200)."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {"from": {"type": "string"}, "limit": {"type": "integer", "minimum": 1, "maximum": 2000}},
            "required": ["from"],
        },
    },
    {
        "name": "gaia_assert",
        "description": (
            "Check an F# bool expression over `world` once per frame for `frames` frames (game time advances when "
            "frames > 1; undoable). Stops at the first false or exception and reports that frame. `probe` is an "
            "optional F# expression of any type, printed at the end or at the failure (e.g. a position). Example: "
            "predicate `(Simulants.Gameplay / \"Scene\" / \"Ball\").GetPosition world |> fun p -> p.Y > -300.0f`."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {
                "predicate": {"type": "string"},
                "frames": {"type": "integer", "minimum": 1, "maximum": 600},
                "probe": {"type": "string"},
                "advance": {"type": "boolean"},
            },
            "required": ["predicate"],
        },
    },
    {
        "name": "nu_docs",
        "description": "Search Nu API XML documentation (member ids and summaries) by regex/keywords. Returns up to `limit` members.",
        "inputSchema": {
            "type": "object",
            "properties": {"query": {"type": "string"}, "limit": {"type": "integer", "minimum": 1, "maximum": 50}},
            "required": ["query"],
        },
    },
]


def _toolset():
    """NU_GAIA_TOOLSET = eval | inspect | screenshot | frames | all (default): the tools an ablation condition sees."""
    level = os.environ.get("NU_GAIA_TOOLSET", "all")
    if level == "all":
        return [tool["name"] for tool in TOOLS]
    names, order = [], list(TOOLSET_LEVELS)
    if level not in order:
        raise SystemExit(f"NU_GAIA_TOOLSET must be one of {order + ['all']}")
    for key in order[: order.index(level) + 1]:
        names += TOOLSET_LEVELS[key]
    return names


ENABLED = set(_toolset())
_bridge = GaiaBridge()
_docs = None


def _text(obj, is_error=False):
    text = obj if isinstance(obj, str) else json.dumps(obj, indent=1)
    return {"content": [{"type": "text", "text": text}], "isError": is_error}


def _png(image):
    buf = io.BytesIO()
    image.save(buf, format="PNG", optimize=True)
    return {"type": "image", "data": base64.b64encode(buf.getvalue()).decode("ascii"), "mimeType": "image/png"}


def _call(op, **args):
    reply = _bridge.request(op, **args)
    if not reply.get("ok"):
        raise RuntimeError(reply.get("error", "bridge error"))
    return reply


def _engine_capture(max_width):
    """Read the next rendered frame back from Gaia's Vulkan renderer (exact pixels; no X server needed)."""
    from PIL import Image

    reply = _call("capture", settle=2)
    image = Image.open(io.BytesIO(base64.b64decode(reply["png"]))).convert("RGB")
    if max_width and image.width > max_width:
        image = image.resize((max_width, round(image.height * max_width / image.width)))
    return image


def _capture(view, max_width):
    if view == "game":
        _call("view_mode", mode="capture")
    try:
        if CAPTURE == "engine":
            image = _engine_capture(max_width)
        else:
            status = _call("wait_frames", frames=2)
            image = screenshot(max_width=max_width, window=status.get("window"))  # crop to Gaia's window
    except Exception:
        if view == "game":
            try:
                _call("view_mode", mode="normal")
            except Exception:  # noqa: BLE001 - keep the original error, not the restore failure
                pass
        raise
    if view == "game":
        _call("view_mode", mode="normal")
    return image


def _save(image, stem):
    os.makedirs(SHOT_DIR, mode=0o700, exist_ok=True)
    path = os.path.join(SHOT_DIR, stem + ".png")
    image.save(path)
    return path


def _restart():
    """Supervisor action: stop the Gaia process recorded by run_gaia_headless.sh and relaunch it on the same project.
    Only a process whose command line is Nu.Gaia.dll is signalled (guards against a recycled pid)."""
    check_private(LOG_DIR, is_dir=True)  # gaia.project decides what code runs; trust only a private log dir
    pid_path = os.path.join(LOG_DIR, "gaia.pid")
    project_path = os.path.join(LOG_DIR, "gaia.project")
    stopped = None
    _bridge.close()
    if os.path.exists(pid_path):
        pid = int(open(pid_path).read().strip())
        try:
            cmdline = open(f"/proc/{pid}/cmdline", "rb").read()
            environ = open(f"/proc/{pid}/environ", "rb").read().split(b"\0")
        except OSError:
            cmdline, environ = b"", []
        # signal only the instance this launcher started: Nu.Gaia.dll bound to *our* bridge socket
        ours = ("NU_GAIA_AGENT_SOCKET=" + _bridge.path).encode() in environ
        if b"Nu.Gaia.dll" in cmdline and ours:
            try:
                os.kill(pid, signal.SIGTERM)
                for _ in range(50):
                    if not os.path.exists(f"/proc/{pid}"):
                        break
                    time.sleep(0.1)
                else:
                    os.kill(pid, signal.SIGKILL)
                    time.sleep(0.5)
            except ProcessLookupError:
                pass  # already gone
            stopped = pid
    cmd = [LAUNCHER]
    if os.path.exists(project_path):
        cmd.append(open(project_path).read().strip())
    done = subprocess.run(cmd, capture_output=True, text=True, timeout=240)
    if done.returncode != 0:
        raise RuntimeError(f"relaunch failed ({done.returncode}): {done.stderr.strip()[-500:]}")
    return {"stopped": stopped, "launcher": done.stdout.strip(), "status": _call("status")}


def _load_docs():
    global _docs
    if _docs is None:
        _docs = []
        if NU_XML and os.path.exists(NU_XML):
            for member in ET.parse(NU_XML).getroot().iter("member"):
                summary = " ".join("".join(member.itertext()).split())
                _docs.append((member.get("name", ""), summary))
    return _docs


def call_tool(name, args):
    if name not in ENABLED:
        return _text(f"unknown tool {name}", is_error=True)
    if name == "gaia_checkpoint":
        return _text(_call("checkpoint", name=args["name"], restore=bool(args.get("restore")), delete=bool(args.get("delete"))))
    if name == "gaia_diff":
        reply = _call("diff", **{"from": args["from"], "limit": int(args.get("limit", 200))})
        return _text({k: reply.get(k) for k in ("from", "added", "removed", "changed", "changedTotal", "truncated")})
    if name == "gaia_assert":
        call = {"predicate": args["predicate"], "frames": int(args.get("frames", 1))}
        if args.get("probe"):
            call["probe"] = args["probe"]
        if args.get("advance") is False:
            call["advance"] = False
        reply = _bridge.request("assert", timeout_s=float(args.get("timeout_s") or 60.0), **call)
        keys = ("passed", "kind", "message", "diagnostics", "checkedFrames", "failedAtFrame", "updateTimeAtFailure",
                "exception", "probe", "frame", "updateTime", "error", "timedOut")
        return _text({k: reply.get(k) for k in keys if reply.get(k) is not None}, is_error=not reply.get("ok") or reply.get("kind") == "compile_error")
    if name == "gaia_status":
        return _text(_call("status"))
    if name == "gaia_eval":
        reply = _bridge.request("eval", timeout_s=float(args.get("timeout_s") or EVAL_TIMEOUT_S), code=args["code"])
        failed = not reply.get("ok") or reply.get("kind") != "ok"
        keys = ("kind", "output", "warnings", "exception", "diagnostics", "frame", "undoDepth", "error", "timedOut", "withdrawn", "busyOp", "busyMs")
        keep = {k: reply.get(k) for k in keys if reply.get(k)}
        return _text(keep, is_error=failed)
    if name == "gaia_ping":
        side = GaiaBridge(path=_bridge.path, timeout=10.0)  # own connection: independent of any stuck request
        try:
            reply = side.request("ping")
        finally:
            side.close()
        return _text(reply, is_error=bool(reply.get("unresponsive") or reply.get("pendingException")))
    if name == "gaia_resolve_exception":
        return _text(_call("resolve_exception", choice=args["choice"]))
    if name == "gaia_restart":
        if args.get("confirm") is not True:
            return _text("gaia_restart requires confirm=true", is_error=True)
        return _text(_restart())
    if name in ("gaia_undo", "gaia_redo"):
        return _text(_call(name[5:]))
    if name == "gaia_advance":
        return _text(_call("advance", frames=int(args["frames"])))
    if name == "gaia_hierarchy":
        return _text(_call("hierarchy")["screens"])
    if name == "gaia_inspect":
        return _text(_call("inspect", path=args.get("path", "")))
    if name == "gaia_screenshot":
        image = _capture(args.get("view", "editor"), args.get("max_width", 1280))
        status = _call("status")
        path = _save(image, f"shot-{status['frame']}")
        return {"content": [_png(image), {"type": "text", "text": f"saved {path}; frame={status['frame']} updateTime={status['updateTime']}"}], "isError": False}
    if name == "gaia_frames":
        from PIL import Image

        count, every, view = int(args["count"]), int(args["every"]), args.get("view", "game")
        frames = []
        for i in range(count):
            if i > 0:
                _call("advance", frames=every)
            frames.append(_capture(view, 480))
        cols = min(4, count)
        rows = (count + cols - 1) // cols
        w, h = frames[0].size
        sheet = Image.new("RGB", (cols * w, rows * h))
        for i, frame in enumerate(frames):
            sheet.paste(frame, ((i % cols) * w, (i // cols) * h))
        status = _call("status")
        path = _save(sheet, f"frames-{status['frame']}")
        return {"content": [_png(sheet), {"type": "text", "text": f"{count} frames, {every} apart; saved {path}"}], "isError": False}
    if name == "gaia_reload_code":
        reply = _call("reload_code")
        return _text(reply.get("log", ""), is_error="Failed to compile" in reply.get("log", ""))
    if name == "gaia_log":
        return _text(_call("log", since=int(args.get("since", 0))).get("log", ""))
    if name == "gaia_symbol":
        reply = _call("symbol", name=args["name"])
        keep = {k: reply.get(k) for k in ("fullName", "assembly", "declaration", "overloads", "diagnostics")}
        return _text(keep, is_error=not reply.get("overloads"))
    if name == "nu_docs":
        docs = _load_docs()
        if not docs:
            return _text("Nu.xml not found; set NU_XML_DOC to Nu/Nu/bin/<config>/<tfm>/Nu.xml", is_error=True)
        words = args["query"].split()
        pats = [re.compile(w, re.I) for w in words]
        scored = []
        for member, summary in docs:
            hay = member + " " + summary
            if all(p.search(hay) for p in pats):
                scored.append((sum(2 if p.search(member) else 1 for p in pats), member, summary))
        scored.sort(key=lambda t: (-t[0], len(t[1])))
        hits = [f"{m}\n    {s}" for _, m, s in scored[: int(args.get("limit", 15))]]
        return _text("\n".join(hits) if hits else "no matches")
    return _text(f"unknown tool {name}", is_error=True)


def handle(msg):
    method, mid = msg.get("method"), msg.get("id")
    if mid is None:
        return None  # notification (e.g. notifications/initialized)
    if method == "initialize":
        result = {
            "protocolVersion": msg.get("params", {}).get("protocolVersion", PROTOCOL_VERSION),
            "capabilities": {"tools": {}},
            "serverInfo": {"name": "nu-gaia", "version": "0.1.0-spike"},
        }
    elif method == "tools/list":
        result = {"tools": [tool for tool in TOOLS if tool["name"] in ENABLED]}
    elif method == "tools/call":
        params = msg.get("params", {})
        try:
            result = call_tool(params.get("name"), params.get("arguments") or {})
        except Exception as exn:  # report tool failures as tool errors, not protocol errors
            result = _text(f"{type(exn).__name__}: {exn}", is_error=True)
    elif method == "ping":
        result = {}
    else:
        return {"jsonrpc": "2.0", "id": mid, "error": {"code": -32601, "message": f"method not found: {method}"}}
    return {"jsonrpc": "2.0", "id": mid, "result": result}


def main():
    for line in sys.stdin:
        if not line.strip():
            continue
        reply = handle(json.loads(line))
        if reply is not None:
            sys.stdout.write(json.dumps(reply) + "\n")
            sys.stdout.flush()


if __name__ == "__main__":
    main()
