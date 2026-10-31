// Gaia - The Nu Game Engine editor.
// Required Notice:
// Copyright (C) Bryan Edds.
// Gaia - The Nu Game Engine editor is licensed under the Nu Game Engine Noncommercial License.
// See: https://github.com/bryanedds/Nu/master/License.md

// EXPERIMENTAL (agentic-gaia spike): a local, opt-in request bridge that lets an external agent drive Gaia.
// Transport only: requests are read on background threads and queued; Gaia drains the queue on the main
// (editor) thread once per frame, so no world or FSI access ever happens off the main thread.

namespace Nu.Gaia
open System
open System.Collections.Concurrent
open System.IO
open System.Net.Sockets
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Prime
open Nu

/// A compiler diagnostic from an interactive evaluation, flattened for JSON.
type EvalDiagnostic =
    { Severity : string
      Code : string
      File : string
      StartLine : int
      StartColumn : int
      EndLine : int
      EndColumn : int
      Message : string }

    static member make (diag : FSharp.Compiler.Diagnostics.FSharpDiagnostic) =
        { Severity = string diag.Severity
          Code = diag.ErrorNumberText
          File = diag.FileName
          StartLine = diag.StartLine
          StartColumn = diag.StartColumn
          EndLine = diag.EndLine
          EndColumn = diag.EndColumn
          Message = diag.Message }

/// The structured outcome of one interactive evaluation.
type EvalOutcome =
    { Kind : string // "ok" | "compile_error" | "runtime_error"
      Output : string
      Warnings : string
      Exception : string
      Diagnostics : EvalDiagnostic array }

    static member empty =
        { Kind = "none"; Output = ""; Warnings = ""; Exception = ""; Diagnostics = [||] }

/// A request from an external agent, completed on the editor thread.
type AgentRequest =
    { Id : string
      Op : string
      Args : JsonElement
      Reply : TaskCompletionSource<string> }

[<RequireQualifiedAccess>]
module AgentBridge =

    /// Environment variable naming the Unix domain socket path. The bridge is off unless it is set.
    let [<Literal>] SocketEnvVar = "NU_GAIA_AGENT_SOCKET"

    /// Upper bound on how long a connection waits for the editor thread to answer one request.
    let ReplyTimeout = TimeSpan.FromSeconds 120.0

    /// Requests waiting for the editor thread.
    let Requests = ConcurrentQueue<AgentRequest> ()

    let mutable private ListenerOpt = Option<Socket>.None

    /// Serialize a reply object to a single JSON line.
    let toJson (value : obj) =
        JsonSerializer.Serialize value

    /// Make an error reply.
    let error (id : string) (message : string) =
        toJson (dict ["id", box id; "ok", box false; "error", box message])

    let private handleConnection (client : Socket) =
        task {
            use stream = new NetworkStream (client, true)
            use reader = new StreamReader (stream, UTF8Encoding false)
            use writer = new StreamWriter (stream, UTF8Encoding false)
            writer.AutoFlush <- true
            let mutable running = true
            while running do
                let! line = reader.ReadLineAsync ()
                if isNull line then running <- false
                elif not (String.IsNullOrWhiteSpace line) then
                    let reply =
                        try use doc = JsonDocument.Parse line
                            let root = doc.RootElement
                            let id = match root.TryGetProperty "id" with | (true, v) -> v.ToString () | _ -> ""
                            let op = match root.TryGetProperty "op" with | (true, v) -> v.GetString () | _ -> ""
                            let args = match root.TryGetProperty "args" with | (true, v) -> v.Clone () | _ -> JsonDocument.Parse("{}").RootElement.Clone ()
                            let tcs = TaskCompletionSource<string> TaskCreationOptions.RunContinuationsAsynchronously
                            Requests.Enqueue { Id = id; Op = op; Args = args; Reply = tcs }
                            if tcs.Task.Wait ReplyTimeout
                            then tcs.Task.Result
                            else error id ("Timed out after " + string ReplyTimeout.TotalSeconds + " s waiting for the editor thread (is an eval still running?).")
                        with exn -> error "" ("Bad request: " + exn.Message)
                    do! writer.WriteLineAsync reply
        } :> Task

    /// Start listening if the socket environment variable is set. Idempotent.
    let tryStart () =
        match (Environment.GetEnvironmentVariable SocketEnvVar, ListenerOpt) with
        | (null, _) | ("", _) | (_, Some _) -> ()
        | (socketPath, None) ->
            try if File.Exists socketPath then File.Delete socketPath
                let listener = new Socket (AddressFamily.Unix, SocketType.Stream, ProtocolType.Unspecified)
                listener.Bind (UnixDomainSocketEndPoint socketPath)
                // owner-only access: on a shared machine an eval socket is arbitrary code execution as this user
                if not (OperatingSystem.IsWindows ()) then
                    File.SetUnixFileMode (socketPath, UnixFileMode.UserRead ||| UnixFileMode.UserWrite)
                listener.Listen 4
                ListenerOpt <- Some listener
                let acceptLoop () =
                    while true do
                        let client = listener.Accept ()
                        handleConnection client |> ignore<Task>
                let thread = Thread (acceptLoop, IsBackground = true, Name = "GaiaAgentBridge")
                thread.Start ()
                Log.info ("Agent bridge listening on " + socketPath)
            with exn -> Log.error ("Agent bridge failed to start: " + exn.Message)
