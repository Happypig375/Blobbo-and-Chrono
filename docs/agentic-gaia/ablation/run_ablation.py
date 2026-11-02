#!/usr/bin/env python3
"""Pilot ablation of feedback channels (design.md section 7): the same agent, tasks and budget; each condition adds
one channel. Runs are serial (one Gaia at a time) on a copy of Breakout ImSim outside the repository.

Usage:
  run_ablation.py WORK_DIR OUT_DIR validate            # oracle check: pristine sources must fail, references must pass
  run_ablation.py WORK_DIR OUT_DIR run [--reps N] [--model M] [--tasks a,b] [--conditions c,d]
WORK_DIR holds a full copy of "Projects/Breakout ImSim" (with bin/) plus WORK_DIR/pristine/*.fs (original sources).
Results are appended to OUT_DIR/results.jsonl (one line per run; finished runs are skipped on a rerun), with each
run's agent stream in OUT_DIR/runs/<run>.jsonl and its source diff in OUT_DIR/runs/<run>.diff.
"""
import difflib
import json
import os
import random
import shutil
import signal
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
PROTO = os.path.join(HERE, "..", "prototype")
sys.path.insert(0, PROTO)
sys.path.insert(0, HERE)
from gaia_bridge import GaiaBridge, private_runtime_dir  # noqa: E402
from tasks import TASKS, prompt  # noqa: E402

CONDITIONS = ["blind", "eval", "inspect", "screenshot", "frames"]  # cumulative tool sets (gaia_mcp.TOOLSET_LEVELS)
# "<tool set>+verify" (e.g. "frames+verify") adds one sentence to the prompt asking the agent to check its change in the
# running game; "<tool set>+instructions" instead sends the MCP server instructions (gaia_mcp.INSTRUCTIONS), which ask
# the same. Both are run only on demand (--conditions), not by default.
RUNTIME = private_runtime_dir()
LOG_DIR = os.path.join(RUNTIME, "nu-gaia-logs")
SOCKET = os.path.join(RUNTIME, "nu-gaia-agent.sock")
LAUNCHER = os.path.join(PROTO, "run_gaia_headless.sh")
REPO = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
NU_XML = os.path.join(REPO, "Nu", "Nu", "bin", "Debug", "net10.0", "Nu.xml")
AGENT_TIMEOUT_S = 20 * 60
SOURCE_FILES = ["Assets.fs", "Simulants.fs", "Gameplay.fs", "Breakout.fs", "BreakoutPlugin.fs", "Program.fs", "Breakout ImSim.fsproj"]


def project_dir(work):
    return os.path.join(work, "Breakout ImSim")


def project_dll(work):
    return os.path.join(project_dir(work), "bin", "Debug", "net10.0", "Breakout ImSim.dll")


# --- Gaia lifecycle -------------------------------------------------------------------------------------------------

def gaia_stop():
    pid_path = os.path.join(LOG_DIR, "gaia.pid")
    if not os.path.exists(pid_path):
        return
    pid = int(open(pid_path).read().strip())
    try:
        if b"Nu.Gaia.dll" not in open(f"/proc/{pid}/cmdline", "rb").read():
            return
        os.kill(pid, signal.SIGTERM)
        for _ in range(50):
            if not os.path.exists(f"/proc/{pid}"):
                return
            time.sleep(0.1)
        os.kill(pid, signal.SIGKILL)
        for _ in range(50):
            if not os.path.exists(f"/proc/{pid}"):
                return
            time.sleep(0.1)
    except (OSError, ProcessLookupError):
        pass


def gaia_start(work):
    gaia_stop()
    done = subprocess.run([LAUNCHER, project_dll(work)], capture_output=True, text=True, timeout=300)
    if done.returncode != 0:
        raise RuntimeError("Gaia launch failed: " + done.stderr[-800:])


# --- sources ------------------------------------------------------------------------------------------------------

def starting_text(work, name, task_id):
    """The file as the agent receives it: the original, with the task's seeded bug (if any) planted in Gameplay.fs."""
    text = open(os.path.join(work, "pristine", name), encoding="utf-8-sig").read()
    if task_id and name == "Gameplay.fs":
        for old, new in TASKS[task_id].get("seed", []):
            assert text.count(old) == 1, (task_id, old[:60])
            text = text.replace(old, new)
    return text


def restore_sources(work, task_id=None):
    proj = project_dir(work)
    for name in SOURCE_FILES:
        shutil.copy2(os.path.join(work, "pristine", name), os.path.join(proj, name))
        if task_id and name == "Gameplay.fs" and TASKS[task_id].get("seed"):
            open(os.path.join(proj, name), "w", encoding="utf-8").write(starting_text(work, name, task_id))
    keep = set(json.load(open(os.path.join(work, "pristine", "toplevel.json"))))
    for name in os.listdir(proj):
        if name not in keep:  # files the agent created
            path = os.path.join(proj, name)
            shutil.rmtree(path) if os.path.isdir(path) else os.remove(path)


def source_diff(work, task_id):
    proj, out = project_dir(work), []
    keep = set(json.load(open(os.path.join(work, "pristine", "toplevel.json"))))
    for name in SOURCE_FILES:
        before = starting_text(work, name, task_id).splitlines(keepends=True)
        after = open(os.path.join(proj, name), encoding="utf-8-sig").read().splitlines(keepends=True)
        out += difflib.unified_diff(before, after, "a/" + name, "b/" + name)
    for name in sorted(set(os.listdir(proj)) - keep):
        out.append(f"+++ new file: {name}\n")
    return "".join(out)


def apply_reference(work, task_id):
    path = os.path.join(project_dir(work), "Gameplay.fs")
    text = open(path, encoding="utf-8-sig").read()
    reference = TASKS[task_id]["reference"]
    if callable(reference):
        text = reference(text)
    else:
        for old, new in reference:
            assert text.count(old) == 1, (task_id, old[:60])
            text = text.replace(old, new)
    open(path, "w", encoding="utf-8").write(text)


# --- oracle -------------------------------------------------------------------------------------------------------

def oracle(work, task_id):
    """Fresh Gaia, load the edited sources, go to the gameplay screen, then run the task's checks."""
    gaia_start(work)
    bridge = GaiaBridge(path=SOCKET, timeout=600)

    def call(op, **args):
        return bridge.request(op, timeout_s=300, **args)

    try:
        log = call("reload_code").get("log", "")
        if "Failed to compile" in log or "Code updated." not in log:
            tail = log[log.find("Failed to compile"):][:1500] if "Failed to compile" in log else log[-800:]
            return {"passed": False, "stage": "compile", "detail": tail}
        setup = call("eval", code='open Breakout\nGame.Set "GameState" Gameplay world')
        if setup.get("kind") != "ok":
            return {"passed": False, "stage": "setup", "detail": json.dumps(setup.get("diagnostics") or setup.get("exception"))[:800]}
        for _ in range(40):  # let the screen transition finish (time advances), then check right away
            status = call("advance", frames=5)
            if status.get("selectedScreen") == "Game/Gameplay":
                break
        else:
            return {"passed": False, "stage": "setup", "detail": "gameplay screen never selected"}
        if status.get("pendingException"):
            return {"passed": False, "stage": "runtime", "detail": status["pendingException"][:800]}
        passed, detail = TASKS[task_id]["oracle"](call)
        pending = call("status").get("pendingException")
        if pending:
            return {"passed": False, "stage": "runtime", "detail": (detail + " | exception: " + pending)[:1500]}
        return {"passed": bool(passed), "stage": "oracle", "detail": detail}
    except Exception as exn:  # noqa: BLE001 - a broken run is a failed run, recorded with its reason
        return {"passed": False, "stage": "harness", "detail": f"{type(exn).__name__}: {exn}"[:800]}
    finally:
        bridge.close()
        gaia_stop()


# --- agent --------------------------------------------------------------------------------------------------------

def mcp_config(path, condition):
    env = {
        "NU_GAIA_AGENT_SOCKET": SOCKET,
        "DISPLAY": os.environ.get("GAIA_DISPLAY", ":91"),
        "XAUTHORITY": os.path.join(LOG_DIR, "xauth"),
        "NU_XML_DOC": NU_XML,
        "NU_GAIA_TOOLSET": condition.split("+")[0],
        # Server instructions (gaia_mcp.INSTRUCTIONS) are sent only in "+instructions" conditions; the main pilot ran without.
        "NU_GAIA_INSTRUCTIONS": "1" if condition.endswith("+instructions") else "0",
        "NU_GAIA_SHOT_DIR": os.path.join(RUNTIME, "nu-gaia-ablation-shots"),
        "XDG_RUNTIME_DIR": RUNTIME,
        "PATH": os.environ["PATH"],
        "HOME": os.environ["HOME"],
    }
    config = {"mcpServers": {"gaia": {"command": sys.executable, "args": ["-I", os.path.join(PROTO, "gaia_mcp.py")], "env": env}}}
    json.dump(config, open(path, "w"), indent=1)


def run_agent(work, out, run_id, task_id, condition, model, budget):
    stream_path = os.path.join(out, "runs", run_id + ".jsonl")
    cmd = ["claude", "-p", prompt(task_id, verify=condition.endswith("+verify")), "--model", model, "--output-format", "stream-json", "--verbose",
           "--max-turns", "60", "--max-budget-usd", str(budget), "--permission-mode", "acceptEdits",
           "--setting-sources", "project,local", "--no-session-persistence", "--strict-mcp-config",
           # only file tools (+ ToolSearch, which loads deferred MCP tools): no shell, web, subagents or messaging
           "--tools", "Read,Edit,Write,Glob,Grep,ToolSearch",
           "--disallowedTools", "Bash,BashOutput,KillShell,WebFetch,WebSearch,Task,Agent,NotebookEdit,TodoWrite"]
    # the agent's auto-memory folder for this working directory must not carry notes from one run to the next
    memory = os.path.join(os.environ["HOME"], ".claude", "projects",
                          "".join(ch if ch.isalnum() else "-" for ch in project_dir(work)), "memory")
    if os.path.isdir(memory):
        shutil.rmtree(memory)
    allowed = "Read,Edit,Write,Glob,Grep"
    if condition != "blind":
        config_path = os.path.join(out, "runs", run_id + ".mcp.json")
        mcp_config(config_path, condition)
        cmd += ["--mcp-config", config_path]
        allowed += ",mcp__gaia"
    cmd += ["--allowedTools", allowed]
    t0 = time.time()
    with open(stream_path, "w") as stream:
        try:
            proc = subprocess.run(cmd, cwd=project_dir(work), stdout=stream, stderr=subprocess.STDOUT, timeout=AGENT_TIMEOUT_S,
                                  stdin=subprocess.DEVNULL)
            code = proc.returncode
        except subprocess.TimeoutExpired:
            code = "timeout"
    seconds = time.time() - t0
    result, tools = {}, {}
    for line in open(stream_path, errors="replace"):
        try:
            event = json.loads(line)
        except ValueError:
            continue
        if event.get("type") == "assistant":
            for item in event.get("message", {}).get("content", []):
                if item.get("type") == "tool_use":
                    tools[item["name"]] = tools.get(item["name"], 0) + 1
        elif event.get("type") == "result":
            result = event
    return {
        "exit": code,
        "seconds": round(seconds, 1),
        "cost_usd": result.get("total_cost_usd"),
        "turns": result.get("num_turns"),
        "agent_result": result.get("subtype"),
        "final_text": (result.get("result") or "")[:1500],
        "output_tokens": (result.get("usage") or {}).get("output_tokens"),
        "tool_calls": tools,
        "permission_denials": len(result.get("permission_denials") or []),
        "api_error": result.get("api_error") if result.get("terminal_reason") == "api_error" else None,
    }


# --- main ---------------------------------------------------------------------------------------------------------

def validate(work, out, tasks):
    rows = []
    for task_id in tasks:
        restore_sources(work, task_id)
        base = oracle(work, task_id)
        apply_reference(work, task_id)
        ref = oracle(work, task_id)
        restore_sources(work)
        rows.append({"task": task_id, "pristine": base, "reference": ref, "valid": (not base["passed"]) and ref["passed"]})
        print(json.dumps(rows[-1]), flush=True)
    path = os.path.join(out, "oracle-validation.json")
    previous = [r for r in (json.load(open(path)) if os.path.exists(path) else []) if r["task"] not in tasks]
    json.dump(previous + rows, open(path, "w"), indent=1)
    return all(r["valid"] for r in rows)


def run(work, out, reps, model, tasks, conditions, budget):
    os.makedirs(os.path.join(out, "runs"), exist_ok=True)
    results_path = os.path.join(out, "results.jsonl")
    done = set()
    if os.path.exists(results_path):
        done = {json.loads(line)["run"] for line in open(results_path) if line.strip()}
    plan = [(rep, t, c) for rep in range(1, reps + 1) for t in tasks for c in conditions]
    rng = random.Random(20261102)
    for rep in range(1, reps + 1):  # shuffle within each repetition so conditions do not drift with time
        block = [p for p in plan if p[0] == rep]
        rng.shuffle(block)
        plan = [p for p in plan if p[0] != rep] + block
    for rep, task_id, condition in plan:
        run_id = f"{task_id}-{condition}-r{rep}"
        if run_id in done:
            continue
        restore_sources(work, task_id)
        if condition == "blind":
            gaia_stop()
        else:
            gaia_start(work)
        started = time.strftime("%Y-%m-%d %H:%M:%S")
        agent = run_agent(work, out, run_id, task_id, condition, model, budget)
        gaia_stop()
        if agent["api_error"]:  # e.g. the account's usage limit: not an attempt, so record nothing and stop
            restore_sources(work)
            print(f"{run_id}: API error ({agent['api_error']}): {agent['final_text'][:200]}; stopping, rerun later", flush=True)
            return
        diff = source_diff(work, task_id)
        open(os.path.join(out, "runs", run_id + ".diff"), "w").write(diff)
        verdict = oracle(work, task_id)
        restore_sources(work)
        record = {"run": run_id, "task": task_id, "condition": condition, "rep": rep, "model": model, "started": started,
                  "changed_source": bool(diff.strip()), **agent, "oracle": verdict}
        with open(results_path, "a") as f:
            f.write(json.dumps(record) + "\n")
        print(f"{run_id}: passed={verdict['passed']} stage={verdict['stage']} cost={agent['cost_usd']} turns={agent['turns']} "
              f"{agent['seconds']}s tools={sum(agent['tool_calls'].values())}", flush=True)


def main(argv):
    work, out, mode = argv[1], argv[2], argv[3]
    os.makedirs(out, exist_ok=True)
    opts = dict(zip(argv[4::2], argv[5::2]))
    if not os.path.exists(os.path.join(work, "pristine", "toplevel.json")):
        json.dump(sorted(os.listdir(project_dir(work))), open(os.path.join(work, "pristine", "toplevel.json"), "w"))
    tasks = opts.get("--tasks", ",".join(TASKS)).split(",")
    if mode == "validate":
        sys.exit(0 if validate(work, out, tasks) else 1)
    conditions = opts.get("--conditions", ",".join(CONDITIONS)).split(",")
    run(work, out, int(opts.get("--reps", "2")), opts.get("--model", "claude-haiku-5-5"), tasks, conditions,
        float(opts.get("--budget", "1.0")))


if __name__ == "__main__":
    main(sys.argv)
