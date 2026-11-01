#!/usr/bin/env python3
"""Summarise results.jsonl from run_ablation.py into a Markdown table (stdout) and summary.json.

Usage: summarize.py OUT_DIR
"""
import json
import os
import sys
from collections import defaultdict

CONDITIONS = ["blind", "eval", "inspect", "screenshot", "frames"]


def main(out):
    runs = [json.loads(line) for line in open(os.path.join(out, "results.jsonl")) if line.strip()]
    tasks = sorted({r["task"] for r in runs})
    cell = defaultdict(list)
    for r in runs:
        cell[(r["task"], r["condition"])].append(r)

    def rate(rows):
        return f"{sum(r['oracle']['passed'] for r in rows)}/{len(rows)}" if rows else "-"

    lines = ["| Task | " + " | ".join(CONDITIONS) + " |", "|---|" + "---|" * len(CONDITIONS)]
    for t in tasks:
        lines.append(f"| {t} | " + " | ".join(rate(cell[(t, c)]) for c in CONDITIONS) + " |")
    by_cond = {c: [r for r in runs if r["condition"] == c] for c in CONDITIONS}
    lines.append("| **all** | " + " | ".join(f"**{rate(by_cond[c])}**" for c in CONDITIONS) + " |")

    def mean(values):
        values = [v for v in values if v is not None]
        return sum(values) / len(values) if values else None

    stats = {}
    for c in CONDITIONS:
        rows = by_cond[c]
        stages = defaultdict(int)
        for r in rows:
            if not r["oracle"]["passed"]:
                stages[r["oracle"]["stage"]] += 1
        tools = defaultdict(int)
        for r in rows:
            for name, n in r["tool_calls"].items():
                tools[name] += n
        stats[c] = {
            "runs": len(rows),
            "passed": sum(r["oracle"]["passed"] for r in rows),
            "mean_cost_usd": mean([r["cost_usd"] for r in rows]),
            "mean_turns": mean([r["turns"] for r in rows]),
            "mean_seconds": mean([r["seconds"] for r in rows]),
            "failure_stages": dict(stages),
            "tool_calls": dict(sorted(tools.items(), key=lambda kv: -kv[1])),
            "gaia_calls_per_run": mean([sum(n for k, n in r["tool_calls"].items() if k.startswith("mcp__gaia")) for r in rows]),
        }
    lines += ["", "| Condition | passed | mean cost (USD) | mean turns | mean minutes | Gaia tool calls per run | failures by stage |",
              "|---|---|---|---|---|---|---|"]
    for c in CONDITIONS:
        s = stats[c]
        if not s["runs"]:
            continue
        fmt = lambda v, f: (f % v) if v is not None else "-"  # noqa: E731
        lines.append(f"| {c} | {s['passed']}/{s['runs']} | {fmt(s['mean_cost_usd'], '%.3f')} | {fmt(s['mean_turns'], '%.1f')} | "
                     f"{fmt(s['mean_seconds'] and s['mean_seconds'] / 60, '%.1f')} | {fmt(s['gaia_calls_per_run'], '%.1f')} | "
                     + (", ".join(f"{k} {v}" for k, v in s["failure_stages"].items()) or "none") + " |")
    total_cost = sum(r["cost_usd"] or 0 for r in runs)
    lines += ["", f"{len(runs)} runs; total agent cost ${total_cost:.2f}."]
    json.dump({"runs": len(runs), "total_cost_usd": total_cost, "by_condition": stats}, open(os.path.join(out, "summary.json"), "w"), indent=1)
    print("\n".join(lines))


if __name__ == "__main__":
    main(sys.argv[1])
