#!/usr/bin/env python3
"""Print how two PNG images differ (bounding box, mean and max difference, number of differing pixels).
Usage: imgdiff.py A.png B.png"""
import sys
from PIL import Image, ImageChops, ImageStat
a, b = (Image.open(p).convert("RGB") for p in sys.argv[1:3])
d = ImageChops.difference(a, b)
g = d.convert("L")
n = sum(1 for v in g.getdata() if v > 0)
print({"bbox": d.getbbox(), "mean": [round(m, 3) for m in ImageStat.Stat(d).mean], "max": g.getextrema()[1], "pixels_differing": n, "of": a.size[0]*a.size[1]})
