# E4. Model Context Protocol (MCP): Landscape, Security Threats, and Future Research Directions: reading note (security sections read closely; rest skimmed)

Short answer: a 37-page survey and vision paper. It maps the life of an MCP server in four phases, lists 16 security threats from four kinds of attacker, builds small proof-of-concept demos for them, and gives safeguards per phase. The demos show that attacks are possible; they do not measure how often they work. Almost all threats assume an untrusted third-party MCP server or user. Gaia's server is first-party and local, so only a part applies. The paper does not discuss tools whose whole purpose is to run code (like `gaia_eval`), so it supports the design's safeguards only by analogy.

## Citation
Hou, X., Zhao, Y., Wang, S., & Wang, H. (2026). Model Context Protocol (MCP): Landscape, Security Threats, and Future Research Directions. *ACM Transactions on Software Engineering and Methodology, 35*(10), Article 302 (October 2026), 37 pages. https://doi.org/10.1145/3796519 . Huazhong University of Science and Technology, Wuhan, China. Preprint: arXiv:2503.23278 (not compared). Licence on the PDF: CC BY-ND 4.0. The authors' keywords include "Vision paper". Data and examples: https://github.com/security-pride/MCP_Landscape (not visited).

## Versions
- One version read: the ACM published PDF of the TOSEM article, 37 pages, downloaded by `litfetch` (`docs/agentic-gaia/litfetch-optional.json`). Page footers read "302:N", where N equals the PDF page.
- The arXiv preprint (arXiv:2503.23278) was not compared. The ecosystem tables in this version are "updated to September 2025" (Tables 1, 2, 4).
- Check date: 2026-11-02.
- Page numbers below are PDF pages 1-37 (they match the "302:N" footers).

## Access and read depth
Full text via `litfetch`, campus access.

**Read closely (word for word):**
- Abstract, Introduction and contributions (pp. 1-3).
- Sec. 3.3 server lifecycle, the four phases and their activities (pp. 7-10).
- All of Sec. 5, the threat taxonomy and its 16 threat write-ups, including Table 3 and the proof-of-concept statement (pp. 15-28).
- All of Sec. 6, implications, challenges and recommendations by lifecycle phase (pp. 28-32).
- Sec. 7.3 (related work on MCP security) and Sec. 8 conclusion (pp. 33-34).

**Skimmed (headings, tables and first paragraphs only):**
- Sec. 2 background on tool-use before MCP (pp. 3-5).
- Sec. 3.1-3.2 components and transport (pp. 5-7).
- Sec. 4 landscape: Tables 1, 2 and 4, and the sampling of MCP.so, were read; the use-case descriptions for OpenAI, Cursor and Cloudflare (Sec. 4.2, pp. 13-14) were only skimmed.
- Sec. 7.1-7.2 related work on tool integration and LLM tool risks (pp. 32-33).

**Not read:** the reference list (pp. 34-37) and the cited primary studies (Hasan et al., Zhao et al., Wang et al., Yang et al. MCPSecBench). The attack figures (Figs. 5-14) were read as captions only; the graphics were not viewed.

## What the paper is
A survey plus vision paper with small proof-of-concept (PoC) demos. It is not a measurement study of attack rates and not a user study. Samples:
- 26 MCP server collections catalogued manually as of September 2025 (Sec. 4.1.2, p. 11), with server counts up to 26,404 (Table 2, p. 12).
- 300 servers randomly sampled from MCP.so, of which 30 were not about MCP and 18 were in development or unavailable (p. 11).
- One PoC MCP server per threat type, run in an isolated environment with a custom host built on the official SDK (p. 15). No count of attack trials.

## Key results (with locators)
1. **MCP basics** (Sec. 3, pp. 5-7): a host application runs a client that talks to servers. Servers offer tools, resources and prompts.
2. **Lifecycle** (Sec. 3.3, pp. 7-10): four phases (creation, deployment, operation, maintenance) with 16 activities. The lifecycle is "derived from the official protocol documentation and a systematic analysis of actual MCP operational workflows" (p. 7); the method is not described further.
3. **Threat taxonomy** (Sec. 5, Table 3, p. 15). Sixteen threats under four attacker types:
   - *Malicious developer (7):* namespace typosquatting, tool name conflict, preference manipulation, tool poisoning, rug pulls, cross-server shadowing, command injection or backdoor (pp. 15-21).
   - *External attacker (2):* installer spoofing, indirect prompt injection (pp. 22-23).
   - *Malicious user (4):* credential theft, sandbox escape, tool chaining abuse, unauthorized access (pp. 23-26).
   - *Security flaws (3):* re-deployment of vulnerable versions, post-update privilege persistence, configuration drift (pp. 26-28).
4. **What the PoC does and does not show** (p. 15): they built PoC servers "to demonstrate security risks and feasibility rather than to evaluate attack success rates". For example, an agent picked between two servers named `github-mcp` and `mcp-github` using only names and descriptions, and the look-alike took tokens (p. 16). Other claims, such as that hosts pick self-promoting tools more often, are reported as observations ("we observed", p. 17) with no numbers.
5. **Threats most relevant to a local code-running tool** (details below):
   - *Sandbox escape* (p. 24): MCP has no runtime isolation or privilege control, so safety depends on the host. A localhost MCP server without authentication can be driven by other local processes, even a Chrome extension. Advice: bind to trusted local clients only, require authentication for every connection, restrict the file system to one directory, allow only approved network domains, validate inputs, use containers or process isolation, and monitor.
   - *Tool chaining abuse* (p. 25): several harmless tools, each within its permissions, are chained by the model into data theft (list files, read `.env`, query a database, write to a public path). Advice: no implicit chaining without approval; limit which outputs can flow into which tools; detect "local file access then network send".
   - *Indirect prompt injection* (pp. 22-23): instructions hidden in data that a tool returns, such as a GitHub issue. Advice: treat tool outputs as adversarial.
   - *Command injection or backdoor* (pp. 20-21): a tool that passes its input to a shell. Their example is a `read_file` tool whose path argument carries `;exec=cat /etc/passwd`.
   - *Tool poisoning, rug pulls, shadowing, typosquatting, installer spoofing, vulnerable versions:* supply-chain threats from third-party servers (pp. 15-22, 26-27).
6. **Challenges** (Sec. 6.2, pp. 29-30): no central security oversight; no standard authentication and authorization; limited logging, auditing and runtime introspection in the spec; and, for multi-step workflows, no "state validation, integrity checks, or rollback mechanisms" so small errors can spread.
7. **Safeguards by phase** (Sec. 6.3, pp. 30-32):
   - Creation: generate metadata automatically; validate that declared capabilities do not exceed their scope; separate modules; check input.
   - Deployment: reproducible signed builds; run every server in a sandbox with limited files, network and commands.
   - Operation: inspect model-generated commands against declared capabilities before they reach the server; keep the sandbox on; per-session short-lived tokens; real-time logs of tool name, time and result summary.
   - Maintenance: version history, automatic rollback of failed updates, config validation and review, tamper-evident logs and anomaly alerts.

## Verbatim quotes (checked against the PDF text; page numbers are PDF pages)
1. (p. 15, Sec. 5) "Since the goal of this PoC is to demonstrate security risks and feasibility rather than to evaluate attack success rates or the behavioral differences among various base LLMs, we leave such comprehensive evaluations for future work."
2. (p. 24, Sec. 5.3.2) "If a host exposes an MCP server on localhost without authentication, other local processes, including a Chrome extension, can connect to it and issue privileged commands."
3. (p. 24, Sec. 5.3.2) "The protocol does not include mechanisms for runtime isolation or privilege control."
4. (p. 23, Sec. 5.2.2) "defenses must treat tool outputs as potentially adversarial"

## Limitations and how strong the evidence is
- **Evidence type:** a survey and vision paper with demonstrations. The taxonomy is the authors' construction from the lifecycle; the method for choosing the 16 threats is not stated as a systematic review. The abstract's phrase "real-world case studies" in practice means PoC servers the authors wrote plus references to incidents reported elsewhere.
- **No rates.** No attack success rates, no comparison of models, no tests of the proposed defences. All safeguards are recommendations, not validated.
- **Scope is third-party servers and marketplaces.** The threat model is a user who installs servers from public markets. It does not model a first-party server run by the same person who owns the machine.
- **Date.** Ecosystem counts are from September 2025 and will have moved.
- **Not addressed:** a tool that is meant to execute arbitrary code for the agent (a REPL or eval tool). The nearest categories are "command injection" (a hidden flaw in the server) and "sandbox escape" (weak host isolation). Accidental damage by a well-meaning agent is not a threat class in the paper. Non-terminating code and editor freezes are also not covered.
- The sampling result (48 of 300 listings unusable) is a small side check, not a systematic quality study.

## Relevance to the Gaia agent design
The design already has a safety model (`design.md` §5). This paper supports parts of it and adds some checks.
- **Local access control (supported).** The paper warns about an unauthenticated localhost server reachable by other local processes. Gaia's bridge uses a Unix socket with mode 0600 in a private 0700 directory, never TCP, and checks ownership (§5.1, §5.5). That follows the paper's advice to bind only to trusted local clients and require authentication. The Unix-socket-with-OS-permissions approach is our substitution for token authentication; the paper does not discuss it.
- **Eval is code execution as the user.** The paper does not treat this case. Its closest advice is sandbox the host, restrict files and network, validate input and log. That matches §5.6 (container or VM with no network, on a git worktree) and §5.7 (human gate on `gaia_eval` and `gaia_reload_code`). Because `gaia_eval` can read files and use the network in one call, tool chaining is not even needed to do harm; the isolation has to be at the process or container level, not the tool level.
- **Tool outputs are untrusted input (add).** Indirect prompt injection is the threat most likely to matter in practice. Text can reach the agent through entity and screen names, property values, scene or `.nu*` files from other people, Nu XML docs, log lines, F# exception messages, and text inside screenshots. Add to the design: treat all of these as data, never as instructions, and do not let the agent act on instructions found in a loaded project.
- **Path arguments (add).** Tools with file or path arguments (`gaia_save file`, `gaia_inspect path`) should resolve inside the project directory, reject `..` and symlinks, and never pass input to a shell. The `read_file` command-injection example (p. 21) is the pattern to avoid.
- **Tool names (small add).** Tool shadowing across servers is a risk when an agent host loads several servers. Keeping the `gaia_` prefix helps; the paper recommends full, server-qualified names (p. 17, p. 20).
- **Third-party MCP servers.** The agent host may also load other servers (GitHub, browsers). Typosquatting, tool poisoning and rug pulls apply to those, not to our own `gaia_mcp.py`. Keep our server in the repo, pinned, and not auto-installed.
- **Logging and rollback.** The paper says the MCP spec has weak logging and no rollback. Gaia adds a snapshot before every mutation, `gaia_undo`, and `gaia_log`. A call journal in the MCP server (tool, time, result summary, as in §6.3.3) would fit. Undo covers the world only, not files or FSI definitions (§5.2); the paper's "rollback" advice is about updates, not world state.
- **Latency and liveness, state-preserving reload, alternatives:** not covered by this paper.

## Corrections to the current literature-review.md entry (E4)
Entry: "Route: Scite. Depth: abstract only; the full text is institutional/closed. Paywalled-but-important." / "Finding: lifecycle and threat taxonomy (16 threat scenarios, four attacker types) with safeguards. Informs: safety of an eval-capable MCP server."
- **Depth line is out of date.** The full text has been read (security sections closely; background and landscape sections skimmed).
- **The abstract-level facts are right:** a four-phase lifecycle (with 16 activities, which the entry does not mention), 16 threat scenarios, four attacker types, safeguards. Authors are four people (Hou, Zhao, Wang, Wang); the entry's "et al." is fine. Published as TOSEM 35(10), Article 302, October 2026.
- **"Informs: safety of an eval-capable MCP server" is overstated.** The paper does not discuss a server whose purpose is to execute code. It studies untrusted third-party servers and users. Say instead: "Threat taxonomy for third-party MCP servers; relevant to Gaia mainly for local access control, indirect prompt injection via tool output, path and command injection, tool chaining, and sandboxing the host."
- **Add the evidence strength:** survey and vision paper ("Vision paper" is among its keywords); the PoC demos do not measure attack success; safeguards are untested recommendations.
