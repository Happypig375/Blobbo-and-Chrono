#!/usr/bin/env python3
"""Task pilot of protocol.md (condition 2, amendment 2): the same agent, task and Gaia through MCP (arm M) or through
the Python-free `gaia` command (arm T). Runs are serial; reuses run_ablation.py's work-tree, Gaia and oracle code.

Usage: run_pilot.py WORK_DIR OUT_DIR DEADLINE_EPOCH [--reps 3]
WORK_DIR holds a copy of "Projects/Breakout ImSim" (with bin/) and WORK_DIR/pristine/ (original sources).
Finished runs (OUT_DIR/results.jsonl) are skipped on a rerun. Before every run: quota read, GPU gate, time check.
"""
import json
import os
import shutil
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(os.path.join(HERE, "..", ".."))
ABL = os.path.join(REPO, "docs", "agentic-gaia", "ablation")
sys.path.insert(0, ABL)
import run_ablation as ra  # noqa: E402
from tasks import prompt  # noqa: E402

MODEL = "claude-opus-5-5"
QUOTA = os.path.expanduser("~/workspace/orchestrator/.orchestration/google-auth-supervision-20261109/read_quota.py")
USAGE_TEXT = open(os.path.join(HERE, "idle-run2", "terminal-usage.txt")).read()
SHOTS_M = os.path.join(ra.RUNTIME, "nu-gaia-ablation-shots")  # where gaia_mcp.py saves (ra.mcp_config)
SHOTS_T = os.path.join(ra.RUNTIME, "nu-gaia-pilot-shots")
RESERVE_S = 10 * 60
os.environ["GAIA_GPU_SHARE"] = "1"  # amendment 2: after gpu_gate.sh; the idle buildinggpt-rag service is always on the GPU


LAST_QUOTA = {}


def quota():
    """Fresh reading if possible (4 tries, 30 s apart; the usage endpoint can answer 429). Otherwise the last good
    reading if it is under 20 minutes old (marked stale). None: no usable reading, so the caller stops."""
    for attempt in range(4):
        out = subprocess.run([sys.executable, QUOTA, "--main"], capture_output=True, text=True, timeout=120).stdout
        try:
            q = json.loads(out.strip().splitlines()[-1])
        except (ValueError, IndexError):
            q = {}
        if q.get("status") == "ok":
            LAST_QUOTA.update(q, at=time.time())
            return dict(q, stale_s=0)
        print(f"quota read: {q or out[-200:]} (try {attempt + 1})", flush=True)
        # same account through the project login (the reader checks that both logins are the same account)
        alt = subprocess.run([sys.executable, QUOTA], capture_output=True, text=True, timeout=120).stdout
        try:
            q = json.loads(alt.strip().splitlines()[-1])
        except (ValueError, IndexError):
            q = {}
        if q.get("status") == "ok":
            LAST_QUOTA.update(q, at=time.time())
            return dict(q, stale_s=0, via="project login")
        time.sleep(30)
    if LAST_QUOTA and time.time() - LAST_QUOTA["at"] < 20 * 60:
        return dict(LAST_QUOTA, stale_s=round(time.time() - LAST_QUOTA["at"]))
    return None


def gate():
    done = subprocess.run([os.path.join(HERE, "gpu_gate.sh")], capture_output=True, text=True, timeout=60)
    return done.returncode == 0, done.stdout.strip()


def wait_gate():
    for _ in range(30):  # wait up to 5 min for the gate (e.g. a labmate's short GPU job)
        ok, line = gate()
        if ok:
            break
        time.sleep(10)
    else:
        raise RuntimeError("GPU gate closed: " + line)


def gaia_start(work):
    wait_gate()
    ra.gaia_start(work)


def clear(path):
    if os.path.isdir(path):
        shutil.rmtree(path)


def agent_cmd(task_id, arm, out, run_id, bindir):
    cmd = ["claude", "-p", prompt(task_id), "--model", MODEL, "--output-format", "stream-json", "--verbose",
           "--max-turns", "60", "--permission-mode", "acceptEdits", "--setting-sources", "project,local",
           "--no-session-persistence", "--strict-mcp-config",
           "--disallowedTools", "BashOutput,KillShell,WebFetch,WebSearch,Task,Agent,NotebookEdit,TodoWrite"]
    env = dict(os.environ)
    files = "Read,Edit,Write,Glob,Grep"
    if arm == "M":
        path = os.path.join(out, "runs", run_id + ".mcp.json")
        ra.mcp_config(path, "frames+instructions")  # all 19 tools, server instructions on, engine capture
        config = json.load(open(path))
        config["mcpServers"]["gaia"]["env"]["GAIA_GPU_SHARE"] = "1"  # so gaia_restart can relaunch (as arm T)
        json.dump(config, open(path, "w"), indent=1)
        cmd += ["--tools", files + ",ToolSearch", "--mcp-config", path, "--allowedTools", files + ",mcp__gaia"]
        cmd[cmd.index("--disallowedTools") + 1] += ",Bash"
    else:
        cmd += ["--tools", files + ",Bash", "--allowedTools", files + ",Bash(gaia:*)",
                "--append-system-prompt", USAGE_TEXT, "--add-dir", SHOTS_T]
        env.update({"PATH": bindir + os.pathsep + env["PATH"], "NU_GAIA_AGENT_SOCKET": ra.SOCKET,
                    "NU_XML_DOC": ra.NU_XML, "NU_GAIA_SHOT_DIR": SHOTS_T, "GAIA_LOG_DIR": ra.LOG_DIR})
    return cmd, env


def parse_stream(path):
    result, tools, bash, denials, images_seen, images_made = {}, {}, [], 0, 0, 0
    for line in open(path, errors="replace"):
        try:
            event = json.loads(line)
        except ValueError:
            continue
        kind = event.get("type")
        if kind == "assistant":
            for item in event.get("message", {}).get("content", []):
                if item.get("type") != "tool_use":
                    continue
                name, inp = item["name"], item.get("input", {})
                tools[name] = tools.get(name, 0) + 1
                if name == "Bash":
                    command = inp.get("command", "")
                    bash.append(command[:200])
                    words = command.split()
                    if len(words) > 1 and words[0] == "gaia" and words[1] in ("capture", "frames"):
                        images_made += 1
                if name == "Read" and str(inp.get("file_path", "")).lower().endswith(".png"):
                    images_seen += 1  # arm T: the agent looked at a PNG file
                if name in ("mcp__gaia__gaia_screenshot", "mcp__gaia__gaia_frames"):
                    images_made += 1
        elif kind == "user":
            for item in event.get("message", {}).get("content", []) or []:
                if isinstance(item, dict) and item.get("type") == "tool_result":
                    content = item.get("content")
                    if isinstance(content, list):  # arm M: an image returned inline is in the agent's context
                        images_seen += sum(1 for c in content if isinstance(c, dict) and c.get("type") == "image")
        elif kind == "result":
            result = event
    usage = result.get("usage") or {}
    return {
        "agent_result": result.get("subtype"), "turns": result.get("num_turns"), "duration_ms": result.get("duration_ms"),
        "cost_usd_reported": result.get("total_cost_usd"), "final_text": (result.get("result") or "")[:1500],
        "input_tokens": usage.get("input_tokens"), "cache_creation_tokens": usage.get("cache_creation_input_tokens"),
        "cache_read_tokens": usage.get("cache_read_input_tokens"), "output_tokens": usage.get("output_tokens"),
        "model_usage": result.get("modelUsage"), "tool_calls": tools, "tool_calls_total": sum(tools.values()),
        "bash_commands": bash, "permission_denials": len(result.get("permission_denials") or []),
        "denied": [str(d)[:200] for d in (result.get("permission_denials") or [])][:10],
        "images_made": images_made, "images_seen": images_seen,
        "api_error": result.get("api_error") if result.get("terminal_reason") == "api_error" else None,
    }


def run_agent(work, out, run_id, task_id, arm, bindir):
    cmd, env = agent_cmd(task_id, arm, out, run_id, bindir)
    memory = os.path.join(os.environ["HOME"], ".claude", "projects",
                          "".join(ch if ch.isalnum() else "-" for ch in ra.project_dir(work)), "memory")
    clear(memory)
    clear(SHOTS_M)
    clear(SHOTS_T)
    os.makedirs(SHOTS_T, mode=0o700)
    stream_path = os.path.join(out, "runs", run_id + ".jsonl")
    t0, first = time.time(), None
    with open(stream_path, "w") as stream:
        proc = subprocess.Popen(["nice", "-n", "5", *cmd], cwd=ra.project_dir(work), stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, stdin=subprocess.DEVNULL, env=env, text=True)
        code = None
        try:
            for line in proc.stdout:
                if first is None:
                    first = time.time() - t0
                stream.write(line)
                if time.time() - t0 > ra.AGENT_TIMEOUT_S:
                    proc.kill()
                    code = "timeout"
                    break
            proc.wait(timeout=60)
        except subprocess.TimeoutExpired:
            proc.kill()
            code = "timeout"
        code = code or proc.returncode
    seconds = time.time() - t0
    return {"exit": code, "seconds": round(seconds, 1), "startup_s": round(first or 0, 2), **parse_stream(stream_path)}


def main(argv):
    work, out, deadline = os.path.abspath(argv[1]), os.path.abspath(argv[2]), float(argv[3])  # absolute: the agent runs in the work tree
    opts = dict(zip(argv[4::2], argv[5::2]))
    reps = int(opts.get("--reps", "3"))
    os.makedirs(os.path.join(out, "runs"), exist_ok=True)
    bindir = os.path.join(work, "bin-cli")
    os.makedirs(bindir, exist_ok=True)
    link = os.path.join(bindir, "gaia")
    if not os.path.exists(link):
        os.symlink(os.path.join(REPO, "docs", "agentic-gaia", "prototype", "gaia"), link)
    results_path = os.path.join(out, "results.jsonl")
    done = {json.loads(l)["run"] for l in open(results_path) if l.strip()} if os.path.exists(results_path) else set()
    if os.path.exists(results_path):  # seed the last good quota reading from the newest record (for a restart)
        last = [json.loads(l) for l in open(results_path) if l.strip()][-1]
        at = time.mktime(time.strptime(last["started"], "%Y-%m-%d %H:%M:%S"))
        LAST_QUOTA.update({"five_hour": {"utilization": last["quota_before"]["five_hour"]},
                           "seven_day": {"utilization": last["quota_before"]["seven_day"]}, "at": at})
    plan = [(rep, t, a) for rep in range(1, reps + 1) for t in ("bounce", "rows") for a in ("M", "T")]
    longest = 8 * 60
    commit = subprocess.run(["git", "-C", REPO, "rev-parse", "HEAD"], capture_output=True, text=True).stdout.strip()
    for rep, task_id, arm in plan:
        run_id = f"{task_id}-{arm}-r{rep}"
        if run_id in done:
            continue
        left = deadline - time.time()
        if left < longest + RESERVE_S:
            print(f"STOP before {run_id}: {left / 60:.1f} min left, longest run so far {longest / 60:.1f} min + 10 min reserve", flush=True)
            return
        q = quota()
        if q is None:
            print(f"STOP before {run_id}: no usable quota reading", flush=True)
            return
        five, week = q["five_hour"]["utilization"], q["seven_day"]["utilization"]
        if week >= 93 or five >= 80:  # 93: this project's 2-point forecast from 90 % plus 1 point of the shared lag
            print(f"STOP before {run_id}: quota five-hour {five}% weekly {week}%", flush=True)
            return
        ra.restore_sources(work, task_id)
        gaia_start(work)
        started = time.strftime("%Y-%m-%d %H:%M:%S")
        agent = run_agent(work, out, run_id, task_id, arm, bindir)
        ra.gaia_stop()
        if agent["api_error"]:
            ra.restore_sources(work)
            print(f"{run_id}: API error ({agent['api_error']}); stopping", flush=True)
            with open(os.path.join(out, "api-errors.jsonl"), "a") as f:
                f.write(json.dumps({"run": run_id, **agent}) + "\n")
            return
        diff = ra.source_diff(work, task_id)
        open(os.path.join(out, "runs", run_id + ".diff"), "w").write(diff)
        t_oracle = time.time()
        try:
            wait_gate()
            verdict = ra.oracle(work, task_id)  # starts its own fresh Gaia
        except Exception as exn:  # noqa: BLE001
            verdict = {"passed": False, "stage": "harness", "detail": f"{type(exn).__name__}: {exn}"[:800]}
        ra.restore_sources(work)
        longest = max(longest, agent["seconds"] + (time.time() - t_oracle) + 30)
        record = {"run": run_id, "task": task_id, "arm": arm, "rep": rep, "model": MODEL, "started": started,
                  "commit": commit, "renderer": "nvidia", "quota_before": {"five_hour": five, "seven_day": week, "stale_s": q["stale_s"], "via": q.get("via", "main")},
                  "changed_source": bool(diff.strip()), **agent, "oracle": verdict}
        with open(results_path, "a") as f:
            f.write(json.dumps(record) + "\n")
        print(f"{run_id}: passed={verdict['passed']} stage={verdict['stage']} {agent['seconds']}s turns={agent['turns']} "
              f"tools={agent['tool_calls_total']} out={agent['output_tokens']} seen/made={agent['images_seen']}/{agent['images_made']} "
              f"denials={agent['permission_denials']}", flush=True)
    print("PLAN COMPLETE", flush=True)


if __name__ == "__main__":
    main(sys.argv)
