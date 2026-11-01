"""Pilot ablation tasks on Breakout ImSim: prompt, automatic oracle, and a reference solution for validating the oracle.

Each oracle runs against a FRESH Gaia that has loaded the agent's edited source files (reload_code), so only changes
saved in the project's files count; live edits the agent made in its own editor session do not.
"""
import colorsys
import os
import re

COMMON = (
    "You are working on 'Breakout ImSim', a sample game for the Nu game engine (F#), in the current directory. "
    "Task: {task} "
    "Make the change in the project's F# source files in this directory. The result will be checked automatically "
    "by loading these source files into a fresh editor and playing the game with no keyboard input. Do not rename "
    "files, the namespace, or the screens, groups and entities. "
    "If tools named gaia_* are available, a live Gaia editor (Nu's game editor) is running this project: "
    "gaia_reload_code loads your edited source files into it, and the other gaia_* tools let you inspect and test "
    "the running game. When you are done, reply with one short paragraph saying what you changed."
)

# F# helpers evaluated in the oracle's editor session (after `open Breakout`).
BRICKS = (
    'let __scene = Simulants.Gameplay / "Scene"\n'
    "let __bricks = World.getEntities __scene world |> Seq.filter (fun e -> (e.GetStaticImage world).AssetName = \"Brick\") |> Seq.toList\n"
    '__bricks |> List.map (fun e -> let p = e.GetPosition world in let s = e.GetSize world in let c = e.GetColor world in '
    'sprintf "%g,%g,%g,%g,%g,%g,%g" p.X p.Y s.X s.Y c.R c.G c.B) |> String.concat ";" '
    '|> fun bricks -> System.IO.File.WriteAllText (@"{path}", sprintf "%d|%s" (Simulants.Gameplay.Get<int> "Score" world) bricks)'
)  # FSI shortens long printed strings ("+[1135 chars]"), so the result goes through a private file
BALL = '(Simulants.Gameplay / "Scene" / "Ball")'


def _string_value(reply):
    m = re.search(r'val it: string =\s*"(.*)"', reply.get("output", ""), re.S)
    return m.group(1) if m else None


def read_bricks(call):
    path = os.path.join(os.environ.get("XDG_RUNTIME_DIR") or "/nonexistent", "nu-gaia-oracle-bricks.txt")
    if os.path.exists(path):
        os.remove(path)
    reply = call("eval", code=BRICKS.replace("{path}", path))
    if reply.get("kind") != "ok" or not os.path.exists(path):
        return None, f"brick query failed: {reply.get('kind')} {str(reply.get('diagnostics') or reply.get('exception'))[:300]}"
    text = open(path).read()
    os.remove(path)
    score, _, rest = text.partition("|")
    bricks = [tuple(float(v) for v in item.split(",")) for item in rest.split(";") if item]
    return (int(score), bricks), None


def rows_of(bricks):
    rows = {}
    for x, y, w, h, r, g, b in bricks:
        rows.setdefault(round(y, 1), []).append((x, w, h, r, g, b))
    return dict(sorted(rows.items()))


def oracle_rows(call):
    data, err = read_bricks(call)
    if err:
        return False, err
    score, bricks = data
    rows = rows_of(bricks)
    ys = list(rows)
    total = len(bricks) + score // 100
    gaps = {round(b - a, 1) for a, b in zip(ys, ys[1:])}
    top = max((y + h / 2 for _, y, _, h, *_ in bricks), default=0)
    ok = total == 35 and len(rows) == 7 and gaps == {16.0} and top <= 172.0 and all(round(h) == 16 and round(w) == 64 for _, _, w, h, *_ in bricks)
    return ok, f"bricks+broken={total} rows={len(rows)} gaps={sorted(gaps)} topEdge={top}"


def oracle_speed(call):
    reply = call("assert", frames=300, predicate=f"let v = {BALL}.GetLinearVelocity world in abs (v.Magnitude - 225.0f) < 12.0f",
                 probe=f"{BALL}.GetLinearVelocity world |> fun v -> v.Magnitude")
    ok = reply.get("passed") is True
    return ok, f"passed={reply.get('passed')} failedAt={reply.get('failedAtFrame')} speed={reply.get('probe')} {reply.get('kind') or ''} {str(reply.get('exception') or '')[:200]}"


def oracle_autopilot(call):
    lives = 'Simulants.Gameplay.Get<int> "Lives" world'
    detail = []
    for _ in range(2):  # 2 x 600 frames = 20 s of play without input
        reply = call("assert", frames=600, predicate=f"{lives} = 5", probe=f'({lives}, Simulants.Gameplay.Get<int> "Score" world)')
        detail.append(f"passed={reply.get('passed')} failedAt={reply.get('failedAtFrame')} (lives,score)={reply.get('probe')}")
        if reply.get("passed") is not True:
            return False, "; ".join(detail)
    score = call("eval", code='Simulants.Gameplay.Get<int> "Score" world')
    m = re.search(r"val it: int = (\d+)", score.get("output", ""))
    ok = bool(m) and int(m.group(1)) >= 100
    return ok, "; ".join(detail) + f"; score={m.group(1) if m else '?'}"


HUE_BANDS = [("red", (345, 15)), ("orange", (15, 45)), ("yellow", (45, 75)), ("green", (75, 165)), ("blue", (195, 255)), ("violet", (255, 320))]


def _in_band(hue, band):
    lo, hi = band
    return lo <= hue <= hi if lo < hi else hue >= lo or hue <= hi


def oracle_rainbow(call):
    data, err = read_bricks(call)
    if err:
        return False, err
    _, bricks = data
    rows = rows_of(bricks)
    detail, ok = [], len(rows) == 6
    for (y, members), (name, band) in zip(rows.items(), HUE_BANDS):
        colors = {(round(r, 2), round(g, 2), round(b, 2)) for _, _, _, r, g, b in members}
        r, g, b = next(iter(colors))
        hue, _, sat = colorsys.rgb_to_hls(r, g, b)
        hue_deg = hue * 360
        good = len(colors) == 1 and sat > 0.4 and _in_band(hue_deg, band)
        ok = ok and good
        detail.append(f"y={y}:{name}{'ok' if good else 'X'}(n={len(colors)},h={hue_deg:.0f})")
    return ok, f"rows={len(rows)} " + " ".join(detail)


def oracle_columns(call):
    data, err = read_bricks(call)
    if err:
        return False, err
    score, bricks = data
    rows = rows_of(bricks)
    width = 320.0 / 6
    ok = len(rows) == 6 and len(bricks) + score // 100 == 36
    detail = [f"rows={len(rows)} bricks+broken={len(bricks) + score // 100}"]
    for y, members in rows.items():
        members = sorted(members)
        xs = [m[0] for m in members]
        ws = [m[1] for m in members]
        good = (len(members) == 6 and all(abs(w - width) < 0.5 for w in ws)
                and all(abs((b - a) - width) < 0.5 for a, b in zip(xs, xs[1:]))
                and abs(xs[0] - ws[0] / 2 + 160) < 0.5 and abs(xs[-1] + ws[-1] / 2 - 160) < 0.5)
        ok = ok and good
        if not good:
            detail.append(f"y={y}: n={len(members)} x={[round(x, 1) for x in xs]} w={sorted({round(w, 1) for w in ws})}")
    return ok, " ".join(detail[:4])


BALLS = (
    'let balls = World.getEntities (Simulants.Gameplay / "Scene") world |> Seq.filter (fun e -> (e.GetStaticImage world).AssetName = "Ball") |> Seq.toArray\n'
)


def oracle_ball2(call):
    predicate = (BALLS + 'balls.Length = 2 && balls |> Array.forall (fun e -> abs ((e.GetLinearVelocity world).Magnitude - 150.0f) < 8.0f)'
                 ' && (balls[0].GetPosition world - balls[1].GetPosition world).Magnitude > 1.0f')
    probe = BALLS + 'balls |> Array.map (fun e -> e.Name, e.GetPosition world, (e.GetLinearVelocity world).Magnitude)'
    reply = call("assert", frames=180, predicate=predicate, probe=probe)
    ok = reply.get("passed") is True
    return ok, f"passed={reply.get('passed')} failedAt={reply.get('failedAtFrame')} balls={str(reply.get('probe'))[:300]} {reply.get('kind') or ''} {str(reply.get('exception') or reply.get('diagnostics') or '')[:200]}"


def reference_ball2(text):
    start = text.index("            // declare ball\n")
    end = text.index("            // declare bricks\n")
    block = text[start:end].replace('"Ball"', "ballName").replace("BallOrigin", "origin")
    indented = "".join(("    " + line) if line.strip() else line for line in block.splitlines(keepends=True))
    wrapped = ("            let processBall (ballName : string) (origin : Vector3) =\n" + indented.rstrip() + "\n\n"
               "            processBall \"Ball\" BallOrigin\n"
               "            processBall \"Ball2\" (BallOrigin + v3 48.0f 0.0f 0.0f)\n\n")
    return text[:start] + wrapped + text[end:]


TASKS = {
    "rows": {
        "task": "Add a seventh row of bricks on top of the six existing rows, with the same brick size and spacing. Every brick must stay below the top wall.",
        "oracle": oracle_rows,
        "reference": [("for j in 0 .. dec 6 do", "for j in 0 .. dec 7 do")],
    },
    "speed": {
        "task": "Make the ball move 50% faster than now, at launch, after losing a life, and after every bounce.",
        "oracle": oracle_speed,
        "reference": [("static let BallSpeed = 150.0f", "static let BallSpeed = 225.0f")],
    },
    "autopilot": {
        "task": "Add an autopilot: while the game is being played, the paddle follows the ball's horizontal position (staying between the walls), so the player never loses a life without pressing any keys.",
        "oracle": oracle_autopilot,
        "reference": [(
            "                let paddlePosition = paddle.GetPosition world\n"
            "                if World.isKeyboardKeyDown KeyboardKey.Left world then",
            "                let paddlePosition = paddle.GetPosition world\n"
            "                let ballX = ((screen / \"Scene\" / \"Ball\").GetPosition world).X\n"
            "                paddle.SetPosition (paddlePosition.MapX (fun _ -> max -128.0f (min 128.0f ballX))) world\n"
            "                if World.isKeyboardKeyDown KeyboardKey.Left world then",
        )],
    },
    "rainbow": {
        "task": "Give each row of bricks its own colour instead of random colours: from the bottom row to the top row, red, orange, yellow, green, blue, violet. All bricks in a row share that row's colour.",
        "oracle": oracle_rainbow,
        "reference": [
            ("    static member make position =\n        { Position = position\n          Size = v3 64.0f 16.0f 0.0f\n"
             "          Color = color (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) (Gen.randomf1 0.5f + 0.5f) 1.0f }",
             "    static member make position color =\n        { Position = position\n          Size = v3 64.0f 16.0f 0.0f\n"
             "          Color = color }"),
            ("(Gen.name, Brick.make (v3 (single i * 64.0f - 128.0f) (single j * 16.0f + 64.0f) 0.0f))",
             "(Gen.name, Brick.make (v3 (single i * 64.0f - 128.0f) (single j * 16.0f + 64.0f) 0.0f) "
             "[|Color (1.0f, 0.0f, 0.0f, 1.0f); Color (1.0f, 0.5f, 0.0f, 1.0f); Color (1.0f, 1.0f, 0.0f, 1.0f); "
             "Color (0.0f, 0.8f, 0.0f, 1.0f); Color (0.0f, 0.2f, 1.0f, 1.0f); Color (0.55f, 0.0f, 1.0f, 1.0f)|].[j])"),
        ],
    },
    "columns": {
        "task": "Use six columns of bricks instead of five. In every row the bricks must exactly fill the space between the inner edges of the left and right walls, with no gaps and no overlaps. Keep the six rows as they are.",
        "oracle": oracle_columns,
        "reference": [
            ("          Size = v3 64.0f 16.0f 0.0f\n          Color = color (Gen", "          Size = v3 (320.0f / 6.0f) 16.0f 0.0f\n          Color = color (Gen"),
            ("[for i in 0 .. dec 5 do", "[for i in 0 .. dec 6 do"),
            ("(v3 (single i * 64.0f - 128.0f) (single j * 16.0f + 64.0f) 0.0f)", "(v3 (single i * (320.0f / 6.0f) - 160.0f + 160.0f / 6.0f) (single j * 16.0f + 64.0f) 0.0f)"),
        ],
    },
    "ball2": {
        "task": "Add a second ball, launched at the same time as the first and starting 48 units to the right of the first ball's starting position. It must behave exactly like the first ball: same speed, bounces off the walls, the paddle and the bricks, breaks bricks, and costs a life (then restarts from its own starting position) when it falls below the paddle.",
        "oracle": oracle_ball2,
        "reference": reference_ball2,
    },
}


def prompt(task_id):
    return COMMON.format(task=TASKS[task_id]["task"])
