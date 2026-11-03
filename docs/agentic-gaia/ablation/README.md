# Pilot comparison of feedback channels (design.md section 7)

**Conclusion first.** In this small pilot, giving the agent more ways to see the running game did **not** make it
succeed more often. On six "build a feature" tasks every condition passed every time (a ceiling: the tasks were
too easy for this to show anything). On the one hard bug-fix task ("bounce"), success was about half in every
condition, and failures had one cause: the agent guessed wrong and **did not use** the tools to check its guess.
A follow-up on that task kept all tools and added one sentence to the prompt asking the agent to check its change
in the running game. Success rose from **2 of 6 to 5 of 6**, and the agent then really used the checking tools
(`gaia_assert` in 6 of 6 runs, against 0 of 6 without the sentence). The price was about 3 times the cost and 5
times the time per run. Putting the same advice into the tool server itself (MCP server instructions, which the
agent sees with every task) worked as well: **5 of 6**, at a little lower cost. With 6 runs per arm each difference
is suggestive, not proven (Fisher's exact test, one-sided p = 0.12 for 2/6 against 5/6; p = 0.06 when the two
nudged arms are pooled, 10/12, a comparison chosen after seeing the data).
The lesson for the design is that having the tools is not enough; the agent has to be steered to use them. The
prototype now ships that advice as server instructions. All numbers below come from a
cheap model (Claude Haiku 5.5) and few repeats, so they can show only large effects.

## What was compared
The same agent (Claude Haiku 5.5, run with `claude -p`), the same prompt and the same budget (at most 60 turns,
US$1 and 20 minutes per run) get one task each. Each condition adds one group of Gaia tools (Gaia is the Nu game
editor; the tools are served by `prototype/gaia_mcp.py`, an MCP server; MCP, the Model Context Protocol, is a
standard way to give an agent tools):

| Condition | What the agent has, on top of reading and editing files |
|---|---|
| blind | nothing else: it cannot run the game |
| eval | run F# code in the live editor, step the game, reload edited source files, read the log |
| inspect | + look at the scene and entity properties, look up names and documentation, checkpoint/diff/assert |
| screenshot | + a picture of the editor |
| frames | + a strip of pictures over time ("frames") |
| frames+verify | the same tools as frames, plus one sentence in the prompt: "Before you finish, check in the running game that the change really works ...; if it does not, keep working." |
| frames+instructions | the same tools as frames and the plain prompt, but the tool server sends instructions (`INSTRUCTIONS` in `prototype/gaia_mcp.py`): "Compiling is not checking: after changing the game, reload the code, then check in the running game ... with a test that can fail, such as gaia_assert ..." |

The agent has no shell, so it cannot build or run the game in any way except through the Gaia tools.

**Renderer (protocol).** All runs recorded here (2 Nov 2026) used Gaia on a virtual X display with Mesa lavapipe,
a software Vulkan renderer on the CPU; the NVIDIA GPU route had not been tested yet. The launcher now uses the GPU by
default, so reproducing these runs means setting `GAIA_VULKAN=lavapipe` (as in the commands at the end). A new
study should use the GPU and say so; `results.jsonl` lines written from 3 Nov on carry a `renderer` field.

**Tasks** (all on the Breakout sample game; `tasks.py`):
- six build tasks: add a seventh row of bricks (rows), make the ball 50% faster (speed), an autopilot paddle
  (autopilot), one colour per row (rainbow), six columns instead of five (columns), a second ball (ball2);
- two bug reports, each made by planting one bug in the source (the agent is not told where):
  - **bounce**: "the ball never bounces off anything ... and it wobbles slightly". The planted bug changes one
    operator so the ball's velocity is reset to a new random direction on every frame (`|=` to `@=`; in Nu's
    immediate-mode API `|=` sets a value once, `@=` sets it every frame).
  - **gravity**: "the ball speeds up while it falls and its path curves downwards". The planted bug deletes the
    line that makes the ball ignore gravity.

**Checking results.** A script (the "oracle") loads the agent's edited sources into a fresh Gaia and plays the game
with no input, then checks the outcome from the game state with `gaia_assert`. For example, the bounce check sets
the ball moving straight up and requires that velocity to survive the next 4 frames (a bounce must not be undone);
the gravity check requires the ball's speed to stay within 3 units of 150 for 120 frames. Before any agent ran, every oracle was validated: it fails on the untouched (or bugged)
sources and passes on a reference fix (`oracle-validation.json`).

## Results
75 runs on 2 Nov 2026, total agent cost US$0.99. Pass counts (passed/runs) by task and condition:

| Task | blind | eval | inspect | screenshot | frames | frames+verify | frames+instructions |
|---|---|---|---|---|---|---|---|
| rows | 1/1 | 1/1 | 1/1 | 1/1 | 1/1 | - | - |
| speed | 1/1 | 1/1 | 1/1 | 1/1 | 1/1 | - | - |
| autopilot | 1/1 | 1/1 | 1/1 | 1/1 | 1/1 | - | - |
| rainbow | 1/1 | 1/1 | 1/1 | 1/1 | 1/1 | - | - |
| columns | 1/1 | 1/1 | 1/1 | 1/1 | 1/1 | - | - |
| ball2 | 1/1 | 1/1 | 1/1 | 1/1 | 1/1 | - | - |
| gravity (bug) | 3/3 | 3/3 | 3/3 | 3/3 | 3/3 | - | - |
| **bounce (bug)** | 1/3 | 0/3 | 2/3 | 2/3 | **2/6** | **5/6** | **5/6** |

Per condition, over all its runs (a "turn" is one model step as counted by `claude -p`; "Gaia calls" counts calls to
the Gaia tools):

| Condition | passed | mean cost (US$) | mean turns | mean minutes | Gaia calls per run |
|---|---|---|---|---|---|
| blind | 10/12 | 0.005 | 9.2 | 0.4 | 0.0 |
| eval | 9/12 | 0.006 | 11.3 | 0.6 | 1.5 |
| inspect | 11/12 | 0.007 | 12.1 | 0.7 | 2.5 |
| screenshot | 11/12 | 0.008 | 15.2 | 0.8 | 4.2 |
| frames | 11/15 | 0.009 | 15.3 | 0.8 | 4.7 |
| frames+verify (bounce only) | 5/6 | 0.050 | 59.2 | 7.0 | 44.3 |
| frames+instructions (bounce only) | 5/6 | 0.038 | 51.8 | 5.8 | 38.5 |

The fair comparison for the follow-up is bounce only, all tools in both arms:

| Bounce, all tools | passed | mean cost (US$) | mean minutes | Gaia calls per run | runs that used assert / frames / advanced the game |
|---|---|---|---|---|---|
| without the sentence | 2/6 | 0.015 | 1.3 | 10.2 | 0 / 0 / 2 |
| with "check it in the running game" in the prompt | 5/6 | 0.050 | 7.0 | 44.3 | 6 / 3 / 5 |
| with the same advice as server instructions | 5/6 | 0.038 | 5.8 | 38.5 | 6 / 1 / 6 |

### Why the bounce runs failed
Every failed bounce run made the same change: it deleted the four `Entity.Sensor .= true` lines (a "sensor" body
detects overlaps but does not collide). That guess looks plausible from the bug report but is wrong, because the
ball's velocity is still overwritten every frame. Every passing run changed the planted `@=` back to a
one-time setter (`.=` or `|=`); some passing runs also deleted the sensor lines, which did no harm.

What separated passes from failures was not which tools the agent had, but whether it looked at the running game:
- Without a nudge, most runs in every tool condition called `gaia_reload_code` once (which only shows that the code compiles) and then
  stopped. They never advanced the game or looked at the ball.
- Across the 18 bounce runs without any nudge (no extra sentence, no server instructions), `gaia_screenshot`, `gaia_frames` and `gaia_assert` were
  never called.
- The two deep investigations without the sentence differ in outcome: one (screenshot condition, 50 turns: 14 code
  evaluations and 11 game steps) found the real cause and passed. The other (frames condition, 61 turns: 17 code
  evaluations and 16 game steps) ended by saying that "the ball now bounces off the paddle, walls and bricks in the
  live editor", but the check in a fresh editor showed the velocity was still being overwritten every frame. Looking
  at the game is not the same as checking it: the agent watched the ball without a test that could fail. (I did not
  check whether it misread what it saw or whether the reloaded live editor really behaved differently from a fresh
  one.)

**With the sentence** the agents behaved differently. All six wrote a `gaia_assert` check, five advanced the game,
and three looked at frames. Five found and fixed the real cause. Some still deleted the sensor lines first, saw in
the running game that the ball still did not bounce, and kept looking, which is exactly the loop the tools are
for. The one failure got lost in its own test: its assert predicate also moved the paddle every frame, which
disturbed the physics, and stepping the game then became very slow (about 3 frames per status poll; the cause was
not isolated), so the run hit its turn limit.

**With server instructions** the picture was the same: all six runs advanced the game and wrote a `gaia_assert`
check, and five fixed the real cause. The one failure is instructive. Its own samples showed the ball still moving
down after touching the paddle ("the contact response isn't clearly showing"), but at turn 59 of 60 it stopped
and reported its sensor change as the fix. Checking found the problem; the agent ran out of room to act on it.

Two design lessons follow from the failures: `gaia_assert` predicates should be read-only (today nothing
stops a predicate from changing the world), and stepping many frames needs to be fast or report progress.

### Gravity and the build tasks
The gravity bug passed 15 of 15, with or without the editor: the missing `Gravity .= GravityIgnore` line is easy
to spot by reading code. The six build tasks passed 30 of 30. Agents with tools used them almost only as a compile
check (about one `gaia_reload_code` per run).

## Limits of this pilot (read before citing any number)
- **Small numbers.** 3 to 6 repeats per cell. A difference of one or two runs is noise. Even the clearest
  difference (2/6 against 5/6) has a one-sided Fisher's exact p of 0.12.
- **Turn limit not exact.** Runs are capped at 60 turns, but the cap is enforced by `claude -p` under its own
  counting: two runs stopped at the cap ("error_max_turns", one of them still passed because its fix was already
  saved), while two others reported 75 and 91 turns and finished normally.
- **Ceiling effect.** The build tasks were too easy for this model to show any difference.
- **One cheap model** (Haiku 5.5). Stronger models may use the tools differently.
- **One game.** All tasks are on Breakout, and the tool descriptions use Breakout examples (for example
  `open Breakout`), which may help every tool condition equally.
- **Machine-wide instructions.** The runs load only project settings (`--setting-sources project,local`), but the
  user's global instruction file may still have been loaded in every condition; it is the same for all conditions.
- **Only one build repeat.** Build repeats 2 and 3 were deliberately stopped after repeat 1 passed everywhere, to
  save the shared usage limit; the harness can resume them (`run --reps 3`).
- **A usage-limit incident.** 11 runs on 2 Nov hit the account's 5-hour usage limit (HTTP 429) and ended without
  doing anything. They were removed and rerun; the harness now stops as soon as it sees such an error.

## Files
- `tasks.py`: task texts, planted bugs, reference fixes and oracles.
- `run_ablation.py`: the harness (`validate` checks the oracles; `run` runs the agent; it resumes and skips
  finished runs).
- `summarize.py`: makes the tables above and `summary.json`.
- `results.jsonl`: one line per run (task, condition, pass/fail, cost, turns, time, tool calls).
- `diffs/`: each run's change to the source files.
- `agent-streams.tar.gz`: each run's full agent transcript (`claude -p` stream-json), for checking what the agent did.
- `summary.json`: the per-condition numbers, including tool-call counts.
- `oracle-validation.json`: the oracle checks.

Reproduce (about 2 to 4 minutes per run; needs the built Breakout ImSim project copied to `WORK_DIR`, see the header
of `run_ablation.py`):
```bash
export GAIA_VULKAN=lavapipe   # the recorded runs were software-rendered; omit for a new study on the GPU
python3 -I docs/agentic-gaia/ablation/run_ablation.py WORK_DIR OUT_DIR validate
python3 -I docs/agentic-gaia/ablation/run_ablation.py WORK_DIR OUT_DIR run --reps 3
python3 -I docs/agentic-gaia/ablation/run_ablation.py WORK_DIR OUT_DIR run --reps 6 --tasks bounce --conditions frames,frames+verify,frames+instructions
python3 -I docs/agentic-gaia/ablation/summarize.py OUT_DIR
```
