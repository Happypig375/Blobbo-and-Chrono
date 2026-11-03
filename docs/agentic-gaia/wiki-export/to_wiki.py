#!/usr/bin/env python3
"""Convert docs/agentic-gaia/*.md into GitHub wiki pages.

Usage: to_wiki.py DOCS_DIR WIKI_DIR
Page links become [[Text|Page-Name]]; links to code and study data point at the full-record branch on the fork
(pinned commit). Fenced code blocks are copied unchanged except for the prototype path.
"""
import os
import re
import shutil
import sys

DOCS, WIKI = sys.argv[1], sys.argv[2]
FORK = "https://github.com/Happypig375/Blobbo-and-Chrono"
RECORD = "6e6325488c2e29ad8962969bb3ea3537f898b7b2"  # agentic-gaia, end of round 3
UPSTREAM_BASE = "1adfd54f22024e870e36045010db5d7e4224652c"  # Nu master the investigation's line numbers refer to
IMG_DIR = "images/agentic-gaia"

PAGES = {
    "investigation.md": ("Agentic-Gaia-Investigation", "Investigation"),
    "design.md": ("Agentic-Gaia-Design", "Design"),
    "literature-review.md": ("Agentic-Gaia-Literature-Review", "Literature review"),
    "literature-protocol.md": ("Agentic-Gaia-Literature-Method", "Literature method"),
    "ablation/README.md": ("Agentic-Gaia-Pilot-Study", "Pilot study"),
}
for name in sorted(os.listdir(os.path.join(DOCS, "reading-notes"))):
    if name.endswith(".md"):
        stem = name[:-3]
        PAGES["reading-notes/" + name] = ("Agentic-Gaia-Reading-Note-" + stem, stem.split("-")[0] + " reading note")

STUDY_FILES = {"tasks.py", "run_ablation.py", "summarize.py", "results.jsonl", "summary.json",
               "oracle-validation.json"}


def blob(path, commit=RECORD, lines=None):
    url = f"{FORK}/blob/{commit}/{path}"
    if lines:
        a, _, b = lines.partition("-")
        url += f"#L{a}" + (f"-L{b}" if b else "")
    return url


def page_link(key):
    page, text = PAGES[key]
    return f"[[{text}|{page}]]"


def norm_doc(ref, src_key):
    """Map a doc reference (possibly relative) to a PAGES key, or None."""
    ref = ref.removeprefix("docs/agentic-gaia/")
    base = os.path.dirname(src_key)
    cand = os.path.normpath(os.path.join(base, ref)) if base else ref
    for k in (cand, ref, "reading-notes/" + ref):
        if k in PAGES:
            return k
    return None


def convert_code_span(span, src_key):
    """Return replacement text for a `code span`, or None to keep it."""
    inner = span[1:-1]
    m = re.fullmatch(r"(?:\.\./)*(?:docs/agentic-gaia/)?((?:reading-notes/|ablation/)?[\w.-]+\.md)", inner)
    if m:
        key = norm_doc(m.group(1), src_key)
        if key:
            return page_link(key)
    m = re.fullmatch(r"(?:\.\./)*img/([\w.-]+\.png)", inner) or (
        re.fullmatch(r"(check-[\w-]+\.png)", inner))
    if m:
        return f"[{m.group(1)}]({IMG_DIR}/{m.group(1)})"
    m = re.fullmatch(r"(Nu/[\w./ -]+?\.(?:fs|fsproj|fsx))(?::(\d+(?:-\d+)?))?", inner)
    if m:
        path, lines = m.group(1), m.group(2)
        commit = UPSTREAM_BASE if lines else RECORD
        return f"[{span}]({blob(path, commit, lines)})"
    m = re.fullmatch(r"(?:docs/agentic-gaia/)?(prototype|ablation)/", inner)
    if m:
        return f"[{span}]({FORK}/tree/{RECORD}/docs/agentic-gaia/{m.group(1)})"
    m = re.fullmatch(r"(?:\.\./)*(?:docs/agentic-gaia/)?((?:prototype|ablation)/[\w./-]+|[\w-]+\.(?:tsv|json))", inner)
    if m:
        return f"[{span}]({blob('docs/agentic-gaia/' + m.group(1))})"
    if src_key.startswith("ablation/") and inner in STUDY_FILES:
        return f"[{span}]({blob('docs/agentic-gaia/ablation/' + inner)})"
    if inner in ("check_mcp.py", "check_review_fixes.py", "gaia_mcp.py", "gaia_bridge.py", "run_gaia_headless.sh"):
        return f"[{span}]({blob('docs/agentic-gaia/prototype/' + inner)})"
    return None


LOCAL = [
    (r"`~/workspace/research-workflow-evaluation`", "the lab's research-workflow notes (private)"),
    (r"`~/workspace/research-workflow-evaluation` \(([^)]*)\)", r"the lab's private research-workflow protocol (\1)"),
    (r"`~/workspace/exchange/literature-quota\.tsv`", "a shared usage log on the lab server"),
    (r"`~/workspace/exchange/[\w.-]+\.md`", "a message between two of the lab's projects (private)"),
    (r"`~/papers/inbox/[\w.-]+\.pdf`", "the lab server (not shared)"),
]


def convert_line(line, src_key):
    for pat, rep in LOCAL[1:2] + LOCAL[:1] + LOCAL[2:]:
        line = re.sub(pat, rep, line)
    line = re.sub(r"`[^`\n]+`", lambda m: convert_code_span(m.group(0), src_key) or m.group(0), line)

    # bare (unquoted) mentions of the main docs, e.g. "design.md section 7"
    def bare(m):
        key = norm_doc(m.group(1), src_key)
        return page_link(key) if key else m.group(0)
    line = re.sub(r"(?<![\w/`\[|])((?:\.\./)?(?:investigation|design|literature-review|literature-protocol)\.md)(?![\w`\]])",
                  bare, line)
    return line


def convert(src_key):
    text = open(os.path.join(DOCS, src_key), encoding="utf-8").read()
    out, fenced = [], False
    for line in text.split("\n"):
        if line.lstrip().startswith("```"):
            fenced = not fenced
            out.append(line)
            continue
        if fenced:
            out.append(line.replace("docs/agentic-gaia/prototype/", "Nu/Nu.Gaia/Agent/"))
        else:
            out.append(convert_line(line, src_key))
    body = "\n".join(out).rstrip() + "\n"
    # the wiki shows the page name as the title, so drop a duplicate leading H1 only if it would repeat it
    footer = (f"\n---\n*Converted from [`docs/agentic-gaia/{src_key}`]({blob('docs/agentic-gaia/' + src_key)}) on the "
              f"`agentic-gaia` branch of the fork (the full record, commit {RECORD[:9]}). Study data stays there; this page "
              f"links to it. Part of [[Agentic Gaia|Agentic-Gaia]].*\n")
    return body + footer


os.makedirs(os.path.join(WIKI, IMG_DIR), exist_ok=True)
for key, (page, _) in PAGES.items():
    with open(os.path.join(WIKI, page + ".md"), "w", encoding="utf-8") as f:
        f.write(convert(key))
for name in os.listdir(os.path.join(DOCS, "img")):
    shutil.copy2(os.path.join(DOCS, "img", name), os.path.join(WIKI, IMG_DIR, name))

# report anything that still looks like a repo-relative reference
for key, (page, _) in PAGES.items():
    text = open(os.path.join(WIKI, page + ".md"), encoding="utf-8").read()
    text = re.sub(r"\[[^\]\n]*\]\([^)\n]*\)", "LINK", text)
    for m in re.finditer(r"(?<!\[)`[^`\n]*(?:\.md|prototype/|ablation/|docs/|~/)[^`\n]*`(?!\])", text):
        print(f"left: {page}: {m.group(0)}")
print(f"wrote {len(PAGES)} pages")
