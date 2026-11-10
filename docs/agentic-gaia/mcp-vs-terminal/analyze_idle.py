#!/usr/bin/env python3
"""Summarise an idle_context.sh run: init tools/MCP status and usage per configuration. Usage: analyze_idle.py RUN_DIR"""
import json
import os
import sys

run = sys.argv[1]
walls = {}
for line in open(os.path.join(run, "runs.jsonl")):
    r = json.loads(line)
    walls[r["label"]] = r
rows = []
for label in ("I0", "IT", "IM", "IMF"):
    init, result = {}, {}
    for line in open(os.path.join(run, label + ".jsonl")):
        try:
            m = json.loads(line)
        except ValueError:
            continue
        if m.get("type") == "system" and m.get("subtype") == "init":
            init = m
        if m.get("type") == "result":
            result = m
    u = result.get("usage", {})
    prompt = u.get("input_tokens", 0) + u.get("cache_creation_input_tokens", 0) + u.get("cache_read_input_tokens", 0)
    rows.append({
        "label": label, "rc": walls[label]["rc"], "wall_s": round(walls[label]["wall_s"], 2),
        "tools": init.get("tools"), "mcp_servers": init.get("mcp_servers"),
        "input": u.get("input_tokens"), "cache_creation": u.get("cache_creation_input_tokens"),
        "cache_read": u.get("cache_read_input_tokens"), "output": u.get("output_tokens"), "prompt_total": prompt,
        "reply": result.get("result"), "turns": result.get("num_turns"), "cost_usd_reported": result.get("total_cost_usd"),
        "model_usage": result.get("modelUsage"),
    })
base = rows[0]["prompt_total"]
for r in rows:
    r["prompt_minus_I0"] = r["prompt_total"] - base
offline = {}
for line in open(os.path.join(run, "mcp-offline.jsonl")):
    m = json.loads(line)
    if m.get("id") == 1:
        offline["instructions_chars"] = len(m["result"].get("instructions", ""))
    if m.get("id") == 2:
        tools = m["result"]["tools"]
        offline["tools"] = len(tools)
        offline["tools_list_json_bytes"] = len(json.dumps(tools))
        offline["tool_names"] = [t["name"] for t in tools]
offline["terminal_usage_chars"] = os.path.getsize(os.path.join(run, "terminal-usage.txt"))
summary = {"configs": rows, "offline": offline}
json.dump(summary, open(os.path.join(run, "summary.json"), "w"), indent=1)
for r in rows:
    print(r["label"], "rc", r["rc"], "wall", r["wall_s"], "prompt", r["prompt_total"], "(+%d)" % r["prompt_minus_I0"],
          "in/cc/cr/out", r["input"], r["cache_creation"], r["cache_read"], r["output"], "reply", repr(r["reply"]),
          "tools", r["tools"], "mcp", r["mcp_servers"])
print(json.dumps(offline))
