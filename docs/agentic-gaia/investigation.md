# Agentic Gaia — investigation (findings 1–4)

Checkout: Nu master `1adfd54f2`, branch `agentic-gaia`. Paths are relative to the repo root.
Status markers: **[read]** = I read the code myself; **[survey]** = from a subagent code survey, spot-checked where noted.

## 1. How Gaia's interactive input works today

### Code path [read]
- The FSI host is a single `FSharp.Compiler.Service` `Shell.FsiEvaluationSession` (FCS 43.12.204,
  `Nu/Nu.Gaia/Nu.Gaia.fsproj:23`) held in a module-level mutable, `FsiSession` (`Nu/Nu.Gaia/Gaia.fs:205`), with
  in-memory streams `FsiInStream`/`FsiOutStream`/`FsiErrorStream` (`Gaia.fs:202-204`) and fixed args
  `--debug+ --optimize- --multiemit+ --gui- ...` (`Gaia.fs:199`). It is created when Gaia starts
  (`Gaia.fs:4544`) and disposed when Gaia exits (`Gaia.fs:4546`).
- The **Interactive** window is `imGuiInteractiveWindow` (`Gaia.fs:3242-3366`). The **Eval** button or Ctrl+Enter
  evaluates; **Enter** or Ctrl+Shift+Enter evaluates and then clears the input (`Gaia.fs:3247-3253`).
- On the first eval after a session (re)start (`InteractiveNeedsInitialization`), Gaia evaluates an init script:
  `#r` lines for engine and dependency DLLs, `Nu.Gaia.dll` and the project DLL, then `open System`, `Prime`,
  `Nu`, `Nu.Gaia`, and best-effort `open MyGame` and `open <ProjectName>` (`Gaia.fs:3257-3313`).
- Before each eval Gaia **binds editor values into the session**, but only if their names appear textually
  in the input: `TargetDir`, `ProjectDllPath`, `SelectedScreen`, `SelectedGroup`, `SelectedEntityOpt`,
  `SelectedEntity` (falls back to a `@Sentinel` entity), and `world` (`Gaia.fs:3315-3327`, via
  `FsiSession.AddBoundValue`). A null-option hack covers `SelectedEntityOpt = None` (`Gaia.fs:3319-3322`, `3343-3346`).
- The input is mirrored to a read-only `input.fsx` in the working directory (`Constants.Gaia.InteractiveInputFilePath`,
  `Nu/Nu.Gaia/GaiaConstants.fs:34`; `Gaia.fs:3328-3331`) and passed as the file name to
  `EvalInteractionNonThrowing (text + ";;", "input.fsx")` (`Gaia.fs:3332-3333`), so diagnostics and debug
  info point at that file.

### Threading [read]
- Evaluation is **synchronous on the main (editor/game) thread**, inside the ImGui pass of the frame:
  `World.runWithoutCleanUp tautology ignore ignore imGuiRender imGuiProcess imGuiPostProcess`
  (`Gaia.fs:4473`; the engine signature is `runWithoutCleanUp runWhile preProcess perProcess postProcess
  imGuiProcess imGuiPostProcess firstFrameCallbackOpt world`, `Nu/Nu/World/WorldModule2.fs:1917`).
  `imGuiProcess` (`Gaia.fs:4238`) calls `imGuiInteractiveWindow world` (`Gaia.fs:4288`).
- Consequences: (a) eval code may touch the `world` directly with no locking, because nothing else runs at the
  same time; (b) a long or non-terminating eval **freezes the editor** — there is no timeout or cancellation;
  (c) any external bridge must marshal requests onto this thread (e.g. a queue drained in `imGuiProcess`),
  never call `FsiSession` or `World.*` from a socket thread.
- Gaia's world is mutable in place (`World.setTimeAdvancing false world` returns unit; `world.CurrentState`
  is a snapshot value), so eval side effects are immediate and visible on the next frame.

### Reachable state [read]
- Everything public in Prime, Nu, Nu.Gaia and the project DLL: the `world`, simulants (`Game`, screens,
  groups, entities via `SelectedGroup / "Name"`), `World.*` APIs (create/destroy entities, set properties,
  publish events, render messages, assets/metadata), plus arbitrary .NET (file IO, processes, network).
  Gaia's own editor state is `private` to the `Gaia` module (selection, undo stacks, camera desire) and is
  reachable only through the explicitly bound values above.
- Engine dependencies are referenced by DLL file name, so the session runs against the same loaded
  assemblies as the editor.

### Results and errors [read]
- Success: `FsiOutStream` text (FSI's `val it: T = ...` echo plus anything printed), or `FsiErrorStream` text
  (warnings) if non-empty — **warnings replace the value echo** (`Gaia.fs:3347-3349`). Output is appended to
  `InteractiveOutputStr`, blank lines stripped (`Gaia.fs:3356-3359`), and both streams cleared.
- Failure: `Choice2Of2 exn` → `"Unhandled exception: " + exn` plus diagnostic messages only — **no ranges or
  error codes** on the interactive path (`Gaia.fs:3350-3356`); the code-reload path does format severity,
  code, file and range (`Gaia.fs:1357-1372`).
- Results are **only strings in an ImGui text buffer**: there is no structured value, type or diagnostic
  object returned anywhere an external caller could get it. The `it` value does remain in the FSI session.
- Exceptions thrown later by code that eval installed (e.g. a subscription or dispatcher behaviour) are
  caught by `runWithCleanUpAndErrorProtection` (`Gaia.fs:4472-4492`), which **rewinds the world with
  `tryUndo`, halts time and shows a message box**, then resumes the loop.

### Undo [read]
- Every eval first calls `snapshot (Evaluate InteractiveInputStr) world` (`Gaia.fs:3255`), pushing
  `world.CurrentState` onto `Pasts` (`Gaia.fs:505-508`). Ctrl+Z (`tryUndo`, `Gaia.fs:620-644`) restores it
  with `World.switch`. Undo/redo are **disabled when the project runs in imperative mode**
  (`World.getImperative world`, `Gaia.fs:621`, `647`).
- Undo covers world state only: not FSI session definitions, files written, processes started, or
  static/mutable state outside the world.

### Code reload (the other "eval" path) [read]
- F9 / "Reload Code" (`Gaia.fs:1735`, `2570`) → `tryReloadCode` (`Gaia.fs:1238-1385`): finds the project's
  `.fsproj` at `TargetDir/../../..`, scrapes `<Compile Include>`, package and DLL references by string
  matching, builds an `.fsx` of `#r`/`#load` lines, **disposes and recreates the FSI session**, evaluates the
  script, and on success calls `World.updateLateBindings ... FsiSession.DynamicAssemblies world` to swap
  dispatcher/facet types in the live world; then publishes `CodeReloadEvent`. On failure it logs formatted
  diagnostics. This is Nu's hot-reload path and the natural "apply source edits" step for an agent that edits
  `.fs` files on disk.
- Reload resets the interactive session (`InteractiveOutputStr <- "(fsi session reset)"`), so interactive
  definitions do not survive a code reload.
