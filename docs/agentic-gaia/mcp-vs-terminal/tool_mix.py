#!/usr/bin/env python3
"""Tool mix per arm from the pilot records: MCP tool names, gaia subcommands, other shell commands, PNG reads."""
import collections
import json
import sys

rows = [json.loads(l) for l in open(sys.argv[1]) if l.strip()]
for arm in ("M", "T"):
    tools, sub, other = collections.Counter(), collections.Counter(), collections.Counter()
    for r in rows:
        if r["arm"] != arm:
            continue
        tools.update(r["tool_calls"])
        for c in r["bash_commands"]:
            w = c.split()
            if w[:1] == ["gaia"]:
                sub[w[1] if len(w) > 1 else "?"] += 1
            else:
                other[w[0] if w else "?"] += 1
    print(arm, "tools", dict(tools.most_common()))
    if arm == "T":
        print(arm, "gaia subcommands (first in command)", dict(sub.most_common()))
        print(arm, "other shell commands (first word)", dict(other.most_common()))
print("cost_usd_reported total", round(sum(r["cost_usd_reported"] or 0 for r in rows), 2),
      {a: round(sum(r["cost_usd_reported"] or 0 for r in rows if r["arm"] == a), 2) for a in "MT"})
import re  # noqa: E402
per_run = {r["run"]: len(re.findall(r"(?:^|[;&|(]\s*|\bdo\s+)gaia\s", "\n".join(r["bash_commands"]), re.M)) for r in rows if r["arm"] == "T"}
print("T gaia invocations per run (incl. chained; commands truncated at 200 chars, so a floor)", per_run)
print("T shell tool calls per run", {r["run"]: r["tool_calls"].get("Bash", 0) for r in rows if r["arm"] == "T"})
print("M gaia tool calls per run", {r["run"]: sum(v for k, v in r["tool_calls"].items() if k.startswith("mcp__gaia")) for r in rows if r["arm"] == "M"})
