"""Hand edits applied after to_wiki.py. Usage: wiki_post_edit.py WIKI_DIR"""
import os
import sys

W = sys.argv[1]
EDITS = {
    "Agentic-Gaia-Literature-Review.md": [
        ("(see\n  NEEDS-HUMAN.md)", "(it was\n  rerun after the reset)"),
        ("Zotero, and the rest are labelled \"not yet in Zotero\" (NEEDS-HUMAN item 4).",
         "Zotero, and the rest are labelled \"not yet in Zotero\". (Update, 2 Nov 2026: all 76 are now in Zotero.)"),
        ("a reading note in `reading-notes/`", "a reading note (see [[Reading notes|Agentic-Gaia-Reading-Notes]])"),
    ],
    "Agentic-Gaia-Design.md": [
        ("- **Reproduce**:\n",
         "- **Reproduce** (paths as on the code-only branch, where the tools live in `Nu/Nu.Gaia/Agent/`; on the full-record\n"
         "  branch `agentic-gaia` they are in `docs/agentic-gaia/prototype/`):\n"),
    ],
}
for page, pairs in EDITS.items():
    p = os.path.join(W, page)
    s = open(p, encoding="utf-8").read()
    for old, new in pairs:
        assert s.count(old) == 1, (page, old)
        s = s.replace(old, new)
    open(p, "w", encoding="utf-8").write(s)

# reading-notes index
notes = sorted((f for f in os.listdir(W) if f.startswith("Agentic-Gaia-Reading-Note-")),
               key=lambda f: (f.split("-")[4][0], int(f.split("-")[4][1:])))
lines = ["# Agentic Gaia: reading notes", "",
         "Full-text reading notes for the most important sources of the [[literature review|Agentic-Gaia-Literature-Review]].",
         "Each note records which version was read, how deeply, and quotes with page or section locators. The letter in the",
         "id is the review's group: A = agents building games in engines, B = live programming and fast feedback in game",
         "development, C = execution feedback and debugging for code agents, D = visual feedback, E = the Model Context",
         "Protocol (MCP), F = agent state and checkpoints.", ""]
for f in notes:
    title = open(os.path.join(W, f), encoding="utf-8").readline().lstrip("# ").strip()
    title = title.replace(": reading note", "")
    lines.append(f"- [[{title}|{f[:-3]}]]")
lines += ["", "Part of [[Agentic Gaia|Agentic-Gaia]]."]
open(os.path.join(W, "Agentic-Gaia-Reading-Notes.md"), "w", encoding="utf-8").write("\n".join(lines) + "\n")
print(len(notes), "notes indexed")
