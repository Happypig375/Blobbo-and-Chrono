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
import sys
import xml.etree.ElementTree as ET

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from gaia_bridge import GaiaBridge, screenshot  # noqa: E402

PROTOCOL_VERSION = "2025-06-18"
SHOT_DIR = os.environ.get("NU_GAIA_SHOT_DIR", os.path.join(os.environ.get("XDG_RUNTIME_DIR", "/tmp"), "nu-gaia-shots"))
NU_XML = os.environ.get("NU_XML_DOC")  # path to Nu.xml (generated next to Nu.dll)

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
            "are opened). `world`, `SelectedScreen`, `SelectedGroup`, `SelectedEntity` are bound when mentioned. "
            "Each eval snapshots the world first, so gaia_undo reverts its world changes (not files or FSI definitions). "
            "Returns kind ok|compile_error|runtime_error, FSI output, and diagnostics with line/column. "
            "Keep snippets small; a non-terminating eval freezes the editor."
        ),
        "inputSchema": {"type": "object", "properties": {"code": {"type": "string"}}, "required": ["code"]},
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
        "description": "Hot-reload the project's F# source files from disk into the live world (Gaia F9). Returns the build log.",
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
        "name": "nu_docs",
        "description": "Search Nu API XML documentation (member ids and summaries) by regex/keywords. Returns up to `limit` members.",
        "inputSchema": {
            "type": "object",
            "properties": {"query": {"type": "string"}, "limit": {"type": "integer", "minimum": 1, "maximum": 50}},
            "required": ["query"],
        },
    },
]

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


def _capture(view, max_width):
    if view == "game":
        _call("view_mode", mode="capture")
    try:
        _call("wait_frames", frames=2)
        return screenshot(max_width=max_width)
    finally:
        if view == "game":
            _call("view_mode", mode="normal")


def _save(image, stem):
    os.makedirs(SHOT_DIR, mode=0o700, exist_ok=True)
    path = os.path.join(SHOT_DIR, stem + ".png")
    image.save(path)
    return path


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
    if name == "gaia_status":
        return _text(_call("status"))
    if name == "gaia_eval":
        reply = _bridge.request("eval", code=args["code"])
        failed = not reply.get("ok") or reply.get("kind") != "ok"
        keep = {k: reply.get(k) for k in ("kind", "output", "warnings", "exception", "diagnostics", "frame", "undoDepth", "error") if reply.get(k)}
        return _text(keep, is_error=failed)
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
        result = {"tools": TOOLS}
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
