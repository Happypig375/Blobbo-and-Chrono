#!/usr/bin/env python3
"""Client for Gaia's experimental agent bridge (agentic-gaia spike), plus a small terminal REPL.

The bridge is a Unix domain socket opened by Gaia when NU_GAIA_AGENT_SOCKET is set. Each request is one JSON
line {"id", "op", "args"}; each reply is one JSON line. Requests are executed on Gaia's editor thread.

Usage:
  gaia_bridge.py OP [JSON_ARGS]       one request, print the reply
  gaia_bridge.py repl                 terminal bridge: type F# (end with ';;' on its own line or at line end),
                                      or ':op {json}' for any other op, ':shot PATH' for a screenshot
Standard library only (Pillow is needed only for screenshots).
"""
import json
import os
import socket
import sys
import itertools

DEFAULT_SOCKET = os.path.join(os.environ.get("XDG_RUNTIME_DIR", "/tmp"), "nu-gaia-agent.sock")


class GaiaBridge:
    def __init__(self, path=None, timeout=150.0):
        self.path = path or os.environ.get("NU_GAIA_AGENT_SOCKET", DEFAULT_SOCKET)
        self.timeout = timeout
        self._ids = itertools.count(1)
        self._sock = None
        self._file = None

    def _connect(self):
        if self._sock is None:
            sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            sock.settimeout(self.timeout)
            sock.connect(self.path)
            self._sock = sock
            self._file = sock.makefile("rwb")

    def close(self):
        if self._sock is not None:
            try:
                self._file.close()
                self._sock.close()
            finally:
                self._sock = None
                self._file = None

    def request(self, op, timeout_s=None, **args):
        """Send one request and return its reply. `timeout_s` (1..600) bounds the server-side wait for the editor
        thread; on expiry the server withdraws a not-yet-started request so it never runs late."""
        self._connect()
        msg = {"id": str(next(self._ids)), "op": op, "args": args}
        if timeout_s is not None:
            msg["timeout_s"] = timeout_s
        self._sock.settimeout(max(self.timeout, (timeout_s or 0) + 30))
        try:
            self._file.write((json.dumps(msg) + "\n").encode("utf-8"))
            self._file.flush()
            line = self._file.readline()
        except (OSError, socket.timeout):
            self.close()  # never reuse a connection that may still deliver a stale reply
            raise
        if not line:
            self.close()
            raise ConnectionError("Gaia closed the bridge connection")
        reply = json.loads(line)
        if reply.get("id") not in (msg["id"], ""):  # "" = unparseable request; anything else is a stale reply
            self.close()
            raise ConnectionError(f"reply id {reply.get('id')!r} does not match request id {msg['id']!r}")
        return reply


def screenshot(display=None, path=None, max_width=None, window=None):
    """Grab the X display Gaia renders to (Xvfb in headless runs). Returns a PIL image (and saves it if path).
    `window` is the {"x","y","width","height"} rectangle from a bridge status reply; the grab is cropped to it."""
    from PIL import ImageGrab  # Pillow with XCB support

    image = ImageGrab.grab(xdisplay=display or os.environ.get("DISPLAY"))
    if window and window.get("width", 0) > 0 and window.get("height", 0) > 0:
        left, top = max(0, window["x"]), max(0, window["y"])
        right, bottom = min(image.width, window["x"] + window["width"]), min(image.height, window["y"] + window["height"])
        if right > left and bottom > top:
            image = image.crop((left, top, right, bottom))
    if max_width and image.width > max_width:
        image = image.resize((max_width, round(image.height * max_width / image.width)))
    if path:
        image.save(path)
    return image


def _print(reply):
    if "output" in reply or "diagnostics" in reply:
        kind = reply.get("kind")
        print(f"[{kind}] frame={reply.get('frame')} undoDepth={reply.get('undoDepth')}")
        for key in ("output", "warnings", "exception"):
            if reply.get(key):
                print(f"--- {key}\n{reply[key].rstrip()}")
        for d in reply.get("diagnostics") or []:
            print(f"{d['Severity']} {d['Code']} {d['File']}({d['StartLine']},{d['StartColumn']}): {d['Message']}")
    else:
        print(json.dumps(reply, indent=2))


def repl(bridge):
    print(f"Gaia bridge at {bridge.path}. F# input ends with ';;'. ':op {{json}}' sends an op; ':shot PATH'; ':q' quits.")
    buffer = []
    while True:
        try:
            line = input("... " if buffer else "gaia> ")
        except EOFError:
            break
        if not buffer and line.startswith(":"):
            if line.strip() in (":q", ":quit"):
                break
            if line.startswith(":shot"):
                parts = line.split(maxsplit=1)
                status = bridge.request("wait_frames", frames=2)
                out = parts[1] if len(parts) > 1 else "gaia-shot.png"
                screenshot(path=out, window=status.get("window"))
                print(f"saved {out}")
                continue
            op, _, rest = line[1:].partition(" ")
            _print(bridge.request(op, **(json.loads(rest) if rest.strip() else {})))
            continue
        buffer.append(line)
        if line.rstrip().endswith(";;"):
            code = "\n".join(buffer).rstrip()[:-2]
            buffer = []
            _print(bridge.request("eval", code=code))


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 2
    bridge = GaiaBridge()
    if argv[1] == "repl":
        repl(bridge)
        return 0
    args = json.loads(argv[2]) if len(argv) > 2 else {}
    reply = bridge.request(argv[1], **args)
    print(json.dumps(reply, indent=2))
    return 0 if reply.get("ok") else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
