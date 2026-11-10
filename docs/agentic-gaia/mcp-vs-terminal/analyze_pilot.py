#!/usr/bin/env python3
"""Per-arm, per-task summary of run_pilot.py results. Usage: analyze_pilot.py PILOT_DIR (writes summary.json)."""
import json
import os
import statistics
import sys

pilot = sys.argv[1]
rows = [json.loads(l) for l in open(os.path.join(pilot, "results.jsonl")) if l.strip()]


def med(values):
    values = [v for v in values if v is not None]
    return statistics.median(values) if values else None


import re


def images(run):
    """Recount from the stream (the harness's own counters missed chained `gaia capture` calls and counted a PNG
    Read twice): made = gaia capture/frames occurrences in shell commands + MCP screenshot/frames calls;
    seen = image blocks actually returned to the agent (MCP inline, or a Read of a PNG)."""
    made = seen = 0
    for line in open(os.path.join(pilot, "runs", run + ".jsonl"), errors="replace"):
        try:
            e = json.loads(line)
        except ValueError:
            continue
        msg = e.get("message") if isinstance(e, dict) else None
        for it in (msg.get("content") if isinstance(msg, dict) else None) or []:
            if not isinstance(it, dict):
                continue
            if e.get("type") == "assistant" and it.get("type") == "tool_use":
                made += len(re.findall(r"\bgaia (?:capture|frames)\b", it.get("input", {}).get("command", "") or ""))
                made += it["name"] in ("mcp__gaia__gaia_screenshot", "mcp__gaia__gaia_frames")
            if e.get("type") == "user" and it.get("type") == "tool_result" and isinstance(it.get("content"), list):
                seen += sum(1 for c in it["content"] if isinstance(c, dict) and c.get("type") == "image")
    return made, seen


summary = {"runs": [], "groups": {}}
for r in rows:
    r["images_made"], r["images_seen"] = images(r["run"])
    prompt = (r["input_tokens"] or 0) + (r["cache_creation_tokens"] or 0) + (r["cache_read_tokens"] or 0)
    tools = r["tool_calls"]
    gaia_calls = sum(v for k, v in tools.items() if k.startswith("mcp__gaia__")) + sum(
        1 for c in r["bash_commands"] if c.split()[:1] == ["gaia"])
    summary["runs"].append({
        "run": r["run"], "passed": r["oracle"]["passed"], "stage": r["oracle"]["stage"], "detail": r["oracle"]["detail"][:200],
        "seconds": r["seconds"], "startup_s": r["startup_s"], "turns": r["turns"], "tool_calls": r["tool_calls_total"],
        "gaia_calls": gaia_calls, "toolsearch": tools.get("ToolSearch", 0), "input": r["input_tokens"],
        "cache_creation": r["cache_creation_tokens"], "cache_read": r["cache_read_tokens"], "output": r["output_tokens"],
        "prompt_total": prompt, "images_made": r["images_made"], "images_seen": r["images_seen"],
        "denials": r["permission_denials"],
        "non_gaia_shell": sum(1 for c in r["bash_commands"] if c.split()[:1] != ["gaia"]), "exit": r["exit"], "agent_result": r["agent_result"],
        "cost_usd_reported": r["cost_usd_reported"]})
for task in ("bounce", "rows"):
    for arm in ("M", "T"):
        group = [s for s in summary["runs"] if s["run"].startswith(f"{task}-{arm}-")]
        if not group:
            continue
        summary["groups"][f"{task}-{arm}"] = {
            "n": len(group), "passed": sum(s["passed"] for s in group),
            **{"median_" + k: med([s[k] for s in group]) for k in
               ("seconds", "startup_s", "turns", "tool_calls", "gaia_calls", "input", "cache_creation", "cache_read",
                "output", "prompt_total", "images_made", "images_seen", "denials", "non_gaia_shell")}}
json.dump(summary, open(os.path.join(pilot, "summary.json"), "w"), indent=1)
for s in summary["runs"]:
    print(s["run"], "PASS" if s["passed"] else "FAIL", s["stage"], f"{s['seconds']}s", f"turns={s['turns']}",
          f"tools={s['tool_calls']} gaia={s['gaia_calls']} ts={s['toolsearch']}", f"in/cc/cr/out={s['input']}/{s['cache_creation']}/{s['cache_read']}/{s['output']}",
          f"img seen/made={s['images_seen']}/{s['images_made']}", f"denials={s['denials']} shell-other={s['non_gaia_shell']}", "|", s["detail"][:90])
for k, g in summary["groups"].items():
    print(k, json.dumps(g))
