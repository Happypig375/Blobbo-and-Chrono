import sys
p = sys.argv[1] + "/Agentic-Gaia-Reading-Notes.md"
s = open(p, encoding="utf-8").read()
s = s.replace(
    "id is the review's group: A = agents building games in engines, B = live programming and fast feedback in game\n"
    "development, C = execution feedback and debugging for code agents, D = visual feedback, E = the Model Context\n"
    "Protocol (MCP), F = agent state and checkpoints.",
    "id is the review's section: A = AI agents and game development, B = live programming and REPLs, C = execution\n"
    "feedback and agent interfaces, D = visual feedback, E = MCP (Model Context Protocol, a standard way to give an AI\n"
    "agent tools) and tool design, F = safety, undo and state management.")
s = s.replace(" (abstract only; full text not retrieved)|", " (later read in full; see the note's addendum)|")
for tail in [" (main paper in full; supplement not read)", " (full text)", " (security sections read closely; rest skimmed)"]:
    s = s.replace(tail + "|", "|")
open(p, "w", encoding="utf-8").write(s)
