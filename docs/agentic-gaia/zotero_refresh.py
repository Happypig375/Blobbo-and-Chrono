"""Refresh zotero-status.tsv from the Zotero Web API (read-only key).

Usage: zotero_refresh.py TSV "Top/Sub collection path"
Matches each row to an item in the collection by DOI, arXiv id, then normalised title. Rows not found in the
collection are looked up library-wide by quick search. Adds/updates a 'collection' column. Never writes to Zotero.
"""
import json
import os
import re
import sys
import urllib.parse
import urllib.request

env = dict(line.strip().split("=", 1) for line in open(os.path.expanduser("~/.config/zotero-sync/api.env")) if "=" in line)
USER, KEY = env["ZOTERO_USER_ID"], env["ZOTERO_API_KEY"]
BASE = f"https://api.zotero.org/users/{USER}"


def get_all(path, params=None):
    out, start = [], 0
    while True:
        q = dict(params or {}, limit=100, start=start, format="json")
        req = urllib.request.Request(f"{BASE}/{path}?{urllib.parse.urlencode(q)}",
                                     headers={"Zotero-API-Key": KEY, "Zotero-API-Version": "3"})
        with urllib.request.urlopen(req, timeout=60) as r:
            page = json.load(r)
            total = int(r.headers.get("Total-Results", len(page)))
        out += page
        start += len(page)
        if not page or start >= total:
            return out


def norm(s):
    return re.sub(r"[^a-z0-9]+", " ", (s or "").lower()).strip()


def ids(data):
    doi = (data.get("DOI") or "").lower()
    extra = data.get("extra") or ""
    m = re.search(r"arxiv[:\s]*(\d{4}\.\d{4,5})", extra + " " + (data.get("url") or "") + " " + (data.get("archiveID") or ""), re.I)
    arx = m.group(1) if m else ""
    m = re.search(r"10\.48550/arxiv\.(\d{4}\.\d{4,5})", doi)
    if m:
        arx = m.group(1)
    return doi, arx


def row_ids(ident):
    ident = (ident or "").lower()
    m = re.search(r"(\d{4}\.\d{4,5})", ident) if "arxiv" in ident or re.fullmatch(r"\d{4}\.\d{4,5}", ident) else None
    return ident, (m.group(1) if m else "")


def main(tsv, coll_path):
    cols = get_all("collections")
    by_key = {c["key"]: c["data"] for c in cols}

    def path_of(d):
        parts = [d["name"]]
        while d.get("parentCollection"):
            d = by_key[d["parentCollection"]]
            parts.insert(0, d["name"])
        return "/".join(parts)

    coll = [k for k, d in by_key.items() if path_of(d) == coll_path]
    if not coll:
        sys.exit(f"collection not found: {coll_path}")
    items = [i["data"] for i in get_all(f"collections/{coll[0]}/items/top")]
    print(f"collection {coll[0]}: {len(items)} top-level items")

    lines = open(tsv, encoding="utf-8").read().splitlines()
    header = lines[0].split("\t")
    if "collection" not in header:
        header.append("collection")
    out = ["\t".join(header)]
    found = 0
    for line in lines[1:]:
        c = line.split("\t")
        c += [""] * (len(header) - len(c))
        rec = dict(zip(header, c))
        doi, arx = row_ids(rec["identifier"])
        title = norm(rec.get("title"))
        hit, how = None, ""
        for it in items:
            idoi, iarx = ids(it)
            if doi and idoi and doi == idoi:
                hit, how = it, "doi"
            elif arx and iarx and arx == iarx:
                hit, how = it, "arxiv"
            elif title and norm(it.get("title")) == title:
                hit, how = it, "title"
            if hit:
                break
        if hit:
            found += 1
            rec.update(zotero_status="in Zotero", zotero_key=hit["key"], match_on=how, collection=coll_path)
        else:
            rec["collection"] = rec.get("collection", "") if rec["zotero_status"] == "in Zotero" else ""
        out.append("\t".join(rec[h] for h in header))
    open(tsv, "w", encoding="utf-8").write("\n".join(out) + "\n")
    missing = [l.split("\t")[0] for l in out[1:] if l.split("\t")[header.index("collection")] == ""]
    print(f"matched in collection: {found}/{len(lines) - 1}; not in collection: {' '.join(missing)}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
