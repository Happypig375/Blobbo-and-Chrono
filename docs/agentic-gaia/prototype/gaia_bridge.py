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

    def request(self, op, **args):
        self._connect()
        msg = {"id": str(next(self._ids)), "op": op, "args": args}
        try:
            self._file.write((json.dumps(msg) + "\n").encode("utf-8"))
            self._file.flush()
            line = self._file.readline()
        except (OSError, socket.timeout):
            self.close()
            raise
        if not line:
            self.close()
            raise ConnectionError("Gaia closed the bridge connection")
        return json.loads(line)


def screenshot(display=None, path=None, max_width=None):
    """Grab the X display Gaia renders to (Xvfb in headless runs). Returns a PIL image (and saves it if path)."""
    from PIL import ImageGrab  # Pillow with XCB support

    image = ImageGrab.grab(xdisplay=display or os.environ.get("DISPLAY"))
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
                bridge.request("wait_frames", frames=2)
                out = parts[1] if len(parts) > 1 else "gaia-shot.png"
                screenshot(path=out)
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
