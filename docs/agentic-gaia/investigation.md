# Agentic Gaia — investigation (findings 1–4)

Checkout: Nu master `1adfd54f2`, branch `agentic-gaia`. Paths are relative to the repo root. **Line numbers refer to
upstream `1adfd54f2`** (view with `git show 1adfd54f2:Nu/Nu.Gaia/Gaia.fs`); this branch's edits shift `Gaia.fs` lines.
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
  (`Gaia.fs:4475`; the engine signature is `runWithoutCleanUp runWhile preProcess perProcess postProcess
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
  `InteractiveOutputStr`, blank lines stripped (`Gaia.fs:3357-3360`), and both streams cleared.
- Failure: `Choice2Of2 exn` → `"Unhandled exception: " + exn` plus diagnostic messages only — **no ranges or
  error codes** on the interactive path (`Gaia.fs:3351-3356`); the code-reload path does format severity,
  code, file and range (`Gaia.fs:1357-1372`).
- Results are **only strings in an ImGui text buffer**: there is no structured value, type or diagnostic
  object returned anywhere an external caller could get it. The `it` value does remain in the FSI session.
- Exceptions thrown later by code that eval installed (e.g. a subscription or dispatcher behaviour) are
  caught by `runWithCleanUpAndErrorProtection` (`Gaia.fs:4474-4494`), which **rewinds the world with
  `tryUndo`, halts time and shows a message box**, then resumes the loop.

### Undo [read]
- Every eval first calls `snapshot (Evaluate InteractiveInputStr) world` (`Gaia.fs:3257`), pushing
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

### Linux bug found while doing this [verified]
- On Linux the **first** eval after a session start returned no output: Gaia filters the init echo by
  splitting on `"\r\n"` and dropping lines containing `--> Referenced '`; FSI on Linux emits `\n`, so the whole
  output was one "line" and was dropped (the code already carried `TODO: ensure the use of \r\n also works on
  linux`). Fixed on this branch by normalising to `\n` first (`evalInteractive` in `Gaia.fs`); the
  `SelectedEntityOpt` hack-strip has the same assumption and now handles both endings.

## 2. Interface options

All options share one hard constraint from §1: **every FSI and world call must run on the editor thread**,
between frames. So any external interface is a *transport* plus a *main-thread request queue*; the options differ
in transport, granularity and who the client is.

| Option | What the agent sees | Feedback quality | Safety | Cost / fit |
|---|---|---|---|---|
| **A. Code-gen blind** (status quo: agent edits `.fs`, human presses F9) | files only | none until a human runs it | file-level git only | zero work; no loop |
| **B. Terminal/REPL bridge** (socket → `eval`, line-oriented CLI; `gaia_bridge.py repl`) | FSI text echo + diagnostics | good for code; no images unless the CLI saves files the agent then opens | per-eval world snapshot (undo); no timeout | tiny; works with any agent that has a shell; reuse of FSI muscle memory |
| **C. MCP server, eval-only** (one `eval` tool) | same as B, as structured JSON | as B; images possible | as B | small; agent must know Nu API by heart to inspect anything |
| **D. MCP server, task-shaped tools** (eval + status, hierarchy, inspect, symbol/docs, advance, screenshot, frames, undo, reload) | structured world state, F# signatures + docs, PNG screenshots and contact sheets | best: three independent signals (diagnostics, world state, pixels) | undo per call, frame-bounded time control, owner-only socket | moderate; this spike |
| **E. Pure GUI agent** (computer-use on the Gaia window: screenshots + synthetic mouse/keys) | pixels only | poor grounding (OSWorld-type failure modes) | none beyond Gaia's own undo | no engine changes; slow, brittle |
| **F. Headless engine API without editor** (`World.makeStub` + custom loop) | world state; no pixels (stub renderer draws nothing, see §4) | code/logic only | process-level | good for CI tests of game logic, not for design |
| **G. Language-server route** (FsAutoComplete/FsLangMcp beside Gaia) | project symbols, diagnostics | no live state; does not see session-defined or hot-reloaded types | read-only | complements D for multi-file refactors |

**Transport inside D.** The spike uses a Unix domain socket with JSON lines (`Nu/Nu.Gaia/GaiaAgentBridge.fs`) and a
separate stdio MCP process (`docs/agentic-gaia/prototype/gaia_mcp.py`). Alternatives: an in-process MCP server
(e.g. ModelContextProtocol C# SDK over HTTP/SSE) removes the Python hop but ties MCP SDK versions to Gaia's
dependency set; named pipes are the Windows equivalent of the socket. Keeping Gaia's side a dumb, versioned
JSON-lines protocol lets B (CLI) and D (MCP) share it, so "both" costs little.

**Safety analysis.**
- *Authority*: `eval` is arbitrary code execution as the user (files, processes, network). On this shared server a
  TCP port on 127.0.0.1 would be reachable by every local user, so the spike binds an owner-only (0600) Unix
  socket in `$XDG_RUNTIME_DIR` and is off unless `NU_GAIA_AGENT_SOCKET` is set. Real sandboxing of FSI is not
  available in-process (.NET has no CAS); stronger isolation means running Gaia itself in a container/VM with no
  network and a scratch copy of the project.
- *Undo*: each `eval`, `advance`, `set_advancing` pushes a world snapshot (Gaia's existing `Pasts` stack), so
  `undo` restores world state (verified pixel-identical in the check). It does **not** undo files, FSI
  definitions, static mutable state, or anything in imperative-mode projects (undo disabled there).
- *Timeouts*: the transport times out a request after 120 s, but a non-terminating eval keeps the editor thread
  blocked; FSI cancellation does not interrupt running user code on .NET. The practical guard is a supervisor
  that kills and relaunches Gaia (state persists in the project files and Gaia's state file), plus an agent rule
  of small evals. *Round 2:* this is now built. The editor thread writes a heartbeat; `ping` is answered off the
  editor thread; a timed-out request is withdrawn so it never runs late; requests fail fast after 10 s without a
  frame; `gaia_restart` relaunches Gaia (it must SIGKILL, because SDL turns SIGTERM into an ignored quit event).
  See design.md §5.4.
- *Exceptions after the eval* (e.g. a bad subscription installed by eval) are already caught by Gaia's
  `runWithCleanUpAndErrorProtection`, which rewinds one snapshot and halts (`Gaia.fs`, §1).
- *Human visibility*: agent evals appear in the Interactive window output and as "Evaluate F# Expression" entries
  in Gaia's Timeline (seen in `img/gaia-headless-editor.png`), so a human watching the editor can follow along.

## 3. Exposing types, definitions and documentation

Full survey notes (FCS API names, FsAutoComplete file references): kept from the subagent survey and summarised
here; the key claim was then verified in the live session.

- **FCS from the live session (verified).** `FsiEvaluationSession.ParseAndCheckInteraction code` type-checks a
  fragment against everything the session has loaded and opened; `GetToolTip` and `GetSymbolUseAtLocation` on the
  result give the F# signature, `FSharpXmlDoc` and the declaration range. The spike's `symbol` op does exactly
  this. For `World.createEntity` it returns
  `static member World.createEntity: mountOpt: Entity Address option -> overlayDescriptor: OverlayDescriptor ->
  surnamesOpt: string array option -> group: Group -> world: World -> Entity (requires 'd :> EntityDispatcher)`,
  the summary "Create an entity and add it to the world." and `Nu/Nu/World/WorldModuleEntity.fs:2398` in 0.45 s.
- **XML docs are the host's job.** For compiled code FCS returns only `FromXmlFile (dll, xmlSig)`; the host must
  open the sidecar `.xml` (FsAutoComplete's `TipFormatter.fs` does this with fallbacks). The spike resolves the
  sidecar through the loaded assembly's location; `Nu.xml` is found next to `Nu.dll`.
- **Nu's docs are thin.** `Nu/Nu/Nu.fsproj` and `Nu.Math` generate XML docs; Gaia, `Nu.Pipe`, templates and game
  projects do not. `Nu.xml` has 3,446 members, all summary-only (no `<param>`/`<returns>`); `///` lines are ~6.6% of
  `Nu/Nu` source; game samples have none. Therefore the declaration location matters more than the doc text:
  the agent should read the source around `file:line`. The engine was built from source here, so locations
  point at real files; for NuGet-only users the fallback is SourceLink (Nu's PDBs) or decompilation, as
  FsAutoComplete does (`Sourcelink.fs`, `Decompiler.fs`).
- **Searching.** FsAutoComplete's workspace-symbol search only covers source files, not referenced assemblies.
  For "find me the API for X", the spike's `nu_docs` does a keyword search over `Nu.xml` member ids and
  summaries (cheap, no engine round-trip). A better index walks `ProjectContext.GetReferencedAssemblies()` /
  `AssemblyContent` once per session (FCS entities with F# display names), joined to the XML table, plus the
  game project's source files. Reflection is faster but loses F# shape (curried functions, modules, DU cases).
- **Existing MCP precedents.** No FsAutoComplete MCP server; third-party FsLangMcp (project-centric, LSP proxy),
  Serena (LSP-backed; weak on external dependencies), and Caddis (F# Interactive over MCP inside Rhino/Revit), which
  is the closest precedent to an FSI-hosting editor bridge.
- **Hot-reloaded types.** After `reload_code`, session-defined game types are new dynamic assemblies; FCS queries
  against the session see the current versions, reflection over the AppDomain sees old and new ones.

## 4. Visual feedback

- **Renderer facts [survey, spot-checked].** This Nu revision is **SDL3 + Vulkan only** (no OpenGL path).
  The window is created with `SDL_WINDOW_VULKAN` (`Nu/Nu/Sdl/SdlDeps.fs:38-41`); a device must support the swapchain
  extension, Vulkan 1.3 and BC texture compression (`Nu/Nu/Vulkan/VulkanContext.fs:481-494`). Rendering runs on a
  separate `RendererThread` unless `RunSynchronously` is set in app config (`Nu/Nu/Core/Constants.fs:75`,
  `Nu/Nu/World/World.fs:626-652`). There is **no screenshot/readback code** in the engine; Gaia's F10 "capture
  mode" only hides the editor chrome. `World.makeStub` runs frames without a window but its renderer draws nothing.
- **Headless on this server [verified].** Xvfb on a private display (`:91`; `:0/:77/:78/:99` are taken) plus
  Mesa **lavapipe** (`VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/lvp_icd.x86_64.json`, Mesa 23.2.1) works: Nu logs
  `Selected Vulkan Device llvmpipe (LLVM 15.0.7, 256 bits), Driver v1.3.255` and the full editor renders
  (`img/gaia-headless-editor.png`). The machine has an RTX 4090 D, but NVIDIA's Vulkan driver cannot present to
  an Xvfb surface, and a real NVIDIA X server needs root, so software Vulkan is the no-sudo route. It is
  adequate for 2D and small 3D scenes; expect low frame rates for heavy 3D. It is CPU-hungry: the idle editor
  used ~7 cores (701% CPU), likely because Gaia renders every frame unpaced (`FramePacing = false`). With
  `LP_NUM_THREADS=4` (now the launcher default) it used about 3.7 cores (366%).
- **Capture [verified].** The spike grabs the Xvfb framebuffer with Pillow's XCB `ImageGrab` after letting two
  frames render (`wait_frames`), in ~0.2 s for 1920x1080. "game" view toggles Gaia's capture mode so only the
  viewport is shown. Frame sequences: `gaia_frames` alternates `advance k` with captures and returns one
  contact sheet (`img/check-4-frames.png`: the splash logo fading over 45 frames), a token-cheap stand-in for
  video. ffmpeg is not installed; a static ffmpeg or Pillow GIF/WebP writer would give actual clips.
- **Limitations.** X-level capture includes the root window border around the 1280x720 Gaia window, depends on an
  X display, and cannot distinguish frames precisely beyond "after N editor frames". The robust design is an
  in-engine readback: the final composited image is already in `resolveTexture` (Rgba8, `TransferSrc` usage),
  so a `vkCmdCopyImageToBuffer` into a staging buffer recorded in `VulkanContext.endFrame` when a capture is
  requested, then mapped after the frame fence, would give exact frames with no X dependency (and would work
  with an offscreen/headless surface on the GPU). Not implemented in the spike.
- **Windows comparator.** Not needed, since Linux rendering worked; hkust-workspace was not touched.
