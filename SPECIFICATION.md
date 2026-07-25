This document is written using strict RFC 2119 vocabulary (MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT, RECOMMENDED, MAY, and OPTIONAL).  It serves as the definitive reference for how the Lisp processes, states, and cross-package domains of the system interact, providing the exact invariants needed to write Buttercup BDD tests.

---

# Macher-agent formal specification

## 1. System overview and domain boundaries

Macher-agent is an asynchronous, stateful LLM orchestration layer for Emacs.  It spans three distinct package domains:

1. **The LLM domain (`gptel`)**: Handles network requests, stream parsing, and Finite State Machines (FSMs).
2. **The review domain (`macher`)**: Handles diff generation, patch application, and the user interface for reviewing code modifications.
3. **The agent domain (`macher-agent`)**: The core orchestrator managing Virtual File Systems (VFS), safe Lisp and process execution, tool bounding, and sub-agent buffer lifecycles.

---

## 2. Core data models and cross-domain objects

### 2.1 The virtual file system and workspace objects

* **`macher-agent-workspace`**: The top-level singleton associated with a project root.  It MUST track all VFS buffers via a hash table, physical file modification times (mtimes), local tool registries, and active sub-agents.
* **`macher-agent-session`**: Binds a workspace to a specific FSM execution.  It MUST track pending media injections and temporary sandbox paths.
* **Virtual file system entry native tuple**: The fundamental unit of virtual memory is represented directly as a Lisp cons-cell tuple of the form `(path orig . curr)` for direct native compatibility, where:
  - `path` is the string path or buffer name, accessed via `macher-agent-vfs-entry-path`.
  - `orig` is the baseline state of the file or buffer from disk or initial load, accessed via `macher-agent-vfs-entry-orig`.
  - `curr` is the current, potentially modified, state in the agent's memory, accessed via `macher-agent-vfs-entry-curr`.
  - Constructed using `macher-agent-vfs-make-entry`.

### 2.2 Orchestration objects

* **`macher-agent-task-context`**: Defines the parameters for a sub-agent execution.  It MUST contain the target buffer, skill symbol, and the composed system message.

### 2.3 Tool response contracts

All tools defined via `macher-agent-make-tool` MUST return one of the following struct types, ensuring predictable asynchronous continuation:

* **`macher-agent-process-response`**: Triggers a synchronous, sandboxed operating system command.
* **`macher-agent-delegate-response`**: Triggers parallel sub-agent creation.
* **`macher-agent-nohup-response`**: Triggers an asynchronous, detached operating system command.
* **`macher-agent-lisp-result-response`**: Triggers standard textual data return.

---

## 3. Virtual file system behaviours

### 3.1 State resolution and priority

When a tool or hook requests the active context, the system MUST resolve it in the following order:

1. Passed context argument.
2. Extracted context from the active FSM (`gptel-fsm-info` / `macher-agent--active-fsm`).
3. The buffer-local `macher-agent--persistent-context`.
4. Fallback to the `macher-agent-active-workspaces` global registry using the current directory's Git or project root.

* File reads MUST prioritise `vfs-entry-curr`.  If no VFS entry exists, it MUST read from active Emacs buffers, and finally fallback to the physical disk.
* VFS entries in `macher-context-contents` MUST use canonical expanded absolute paths (resolved via `macher-agent--normalize-path-key`) for file entries to ensure seamless state sharing between `macher.el` and `macher-agent`.  Unvisiting or pure Emacs buffers (such as `*scratch*`) MUST retain raw string names as VFS keys.
* Any path accessed by the VFS MUST NOT be absolute when passed by tools and MUST resolve strictly within the designated workspace root.  Path traversal (for example, `../` escaping the root) MUST throw a `SECURITY ERROR`.

### 3.2 Ephemeral sandboxing

When executing operating system processes (via `macher-agent-process-response`), the system MUST ensure total isolation:

1. The orchestrator MUST verify the project is a valid Git repository.
2. It MUST create an ephemeral operating system directory (`/tmp/macher-sandbox-XXX`).
3. It MUST clone the physical disk state via `git ls-files --recurse-submodules ... | rsync`.
4. It MUST apply the VFS overlay (writing all modified `vfs-entry-curr` strings into the sandbox).
5. The process MUST execute inside this sandbox.  Upon exit (0 or greater than 0), the sandbox MUST be recursively deleted.
6. Path containment verification uses standard Emacs file system functions when paths exist, and falls back to canonicalised lexical prefix matching (implemented in `macher-agent--resolve-safe-path`) when virtual directories do not exist on disk, preventing false positives in the virtual file system sandbox isolation test suite.

### 3.3 Auto-synchronisation

The VFS MUST automatically synchronise its internal `orig` state with the physical disk:

1. Immediately before a `gptel` stream begins (`gptel-pre-response-hook`).
2. After any file is mutated by a tool.
3. If an external disk change modifies the file's `mtime` beyond the agent's baseline, the system MUST throw an error and discard the agent's virtual edit to prevent regressions.

---

## 4. Sub-agent orchestration lifecycle

### 4.1 Sub-agent creation and state inheritance

When a delegated sub-agent buffer is spawned (`macher-agent-add-subagent`), its initial state configuration MUST strictly follow these inheritance and composition rules:

1. **Conditional inheritance:** The sub-agent buffer MUST inherit the parent buffer's LLM configuration (`gptel-model`, `gptel-backend`, `gptel-temperature`, and `gptel-max-tokens`) **ONLY IF** no explicit `presets` are assigned to the delegation task.
2. **Skill composition:** If one or more `presets` are assigned, the system MUST implement pure composition (`macher-agent-compose-payload`).  The composition MUST:
   - Concatenate their individual system prompts into a unified `gptel--system-message`.
   - Override the inherited LLM configuration with the highest-priority preset's defined parameters (for example, model, temperature).
3. **Tool aggregation:** The sub-agent MUST inherit the base `gptel-tools`, but MUST append and deduplicate any additional tools explicitly required by the composed presets.
4. **Buffer-local presets:** The system MUST store active presets in a buffer-local variable `macher-agent-presets` for initialisation seeding.  On prompt transformations, any buffer-local presets MUST be automatically passed to the pure composition engine along with dynamic prompt tags to compute the transmission state ephemerally.
5. **Agent tagging:** The buffer MUST be permanently marked via the local variable `macher-agent--is-subagent` to ensure standard buffer-kill commands cannot bypass the agent reaper.

### 4.2 Parallel execution

The system MUST support parallel sub-agent execution (`macher-agent-execute-parallel`).  The orchestrator MUST wait until all sub-agents trigger their completion callbacks before aggregating the results and returning them to the parent LLM.

### 4.3 Garbage collection and reaping

When an agent flags its execution as complete via `macher-agent-submit-task-result-tool`:

1. The buffer MUST be marked with `macher-agent--ready-to-reap`.
2. A post-response hook (`macher-agent-post-response-reaper`) MUST execute on a `0` timer.
3. The reaper MUST abort any active gptel process associated with the buffer via `gptel-abort`.
4. The reaper MUST delete the buffer without prompting the user.

---

## 5. Skill and tool management invariants

### 5.1 Skill definition and composition

1. **Parsing**: Skills defined in `SKILL.md` MUST contain YAML frontmatter specifying `name`, `description`, `model`, and `allowed-tools`.  The markdown body is treated as the system prompt.
2. **Composition (`@skill`)**: The pre-request hook MUST parse the prompt for `@skill_name` tokens.  If found, it MUST dynamically merge the requested skill's system prompt, tools, and model overrides into the current execution.
3. **Queue formatting**: System directives injected mid-flight via `macher-agent-inject-thought` MUST be appended directly to the next tool's string output, preceded by `=== SYSTEM DIRECTIVE ===`.

### 5.2 Tool abstract syntax tree security

1. When loading `.el` tool scripts from the workspace, the system MUST parse the Lisp Abstract Syntax Tree (AST).
2. The evaluator MUST reject and refuse to load any tool containing top-level mutative forms (`defun`, `defvar`, `defcustom`, `defmacro`, `cl-defun`).  *(Note: Enforced strictly at the AST root).*

### 5.3 Execution scope enforcement

1. Before any tool executes, `macher-agent--enforce-tool-scope` MUST run.
2. The enforcer MUST check if the requested tool exists in the specific FSM's authorised tool list or the buffer-local `gptel-tools` by comparing their canonical string names resolved via `macher-agent-canonical-tool-name`, eliminating multiple type-checking routines in execution hooks.
3. If the tool is not explicitly authorised for that sub-agent, execution MUST be blocked, returning an out-of-scope error to the LLM.

### 5.4 Tool lifecycle and hook architecture

The tool macro `macher-agent-make-tool` automatically instruments generated functions with standard Emacs hooks.  This allows external packages, user configurations, and multi-agent orchestrators to intercept, validate, or mutate tool executions globally.

#### 5.4.1 Exposed lifecycle hooks

The following hooks are exposed globally.  Each hook is called with the tool name (as a symbol) and its evaluated arguments (as a plist):

1. `macher-agent-pre-tool-use-hook`: Executes before any tool logic runs.  Run via `run-hook-with-args-until-failure`.  If any function in this hook returns nil or signals an error, the tool execution is immediately aborted.
2. `macher-agent-permission-request-hook`: Executes after `macher-agent-pre-tool-use-hook` but before the main body.  Designed for interactive approvals.  This hook is permissive by default; if the hook is empty, execution proceeds.  Run via `run-hook-with-args-until-failure`.
3. `macher-agent-post-tool-use-hook`: Executes immediately after the tool body completes successfully.  Run via `run-hook-with-args`.  Receives the tool name, arguments, and the resulting output of the tool.  Used for updating virtual file systems, triggering background linters, or logging.
4. `macher-agent-post-tool-use-failure-hook`: Executes if the tool body throws an Emacs Lisp error.  Run via `run-hook-with-args`.  Receives the tool name, arguments, and the error signal data.  Used for feeding structured failure context back to the LLM.

---

## 6. Upstream bridge contracts

Because `macher-agent` acts as an orchestrator between external packages (`gptel` for LLM networking, `macher` for diff review), it MUST intercept upstream execution flows.  Developers MUST NOT alter these boundary contracts, as upstream packages depend on strict data types and structural integrity.

### 6.1 Cross-boundary object shapes

Developers MUST expect the following upstream data structures when intercepting functions or hooks:

1. **`gptel-fsm` (Finite State Machine):** An upstream struct tracking the LLM request.  `macher-agent` MUST interact with it primarily via `(gptel-fsm-info fsm)`, which returns a Lisp property list (plist).
* *Expected keys:* `:buffer`, `:backend`, `:data`, `:tools`.
* *Injected keys:* `macher-agent` MUST safely inject `:macher-agent-session` and `:macher-agent-context` into this plist.

2. **`gptel-tool`:** An upstream struct.  `macher-agent` relies on `gptel-tool-name` (returns a string or symbol) and `gptel-tool-function` (returns a Lisp closure).
- The extraction and comparison MUST be wrapped in a canonical tool name handler:
```elisp
(let* ((tool-name (macher-agent-canonical-tool-name tool)))
  (string= tool-name "my_target_tool"))
```
3. **`macher-context`:** An upstream struct tracking diff boundaries.  `macher-agent` intercepts its accessors (`macher-context-contents`, `macher-context-prompt`, `macher-context-workspace`).

---

### 6.2 The gptel boundary for network and tool execution

The system MUST intercept `gptel`'s private networking and stream insertion functions to ensure VFS isolation and prevent Emacs crashes on empty network chunks.

#### 6.2.1 Stream insertion null safety

* **Target:** `gptel--insert-response` and `gptel-curl--stream-insert-response`
* **Interception:** `advice-add :around` (`#'macher-agent--protect-nil-responses`)
* **Inputs received:** `(response info &optional raw)` where `response` is a string or nil (upstream network handlers frequently pass nil if a stream chunk drops), and `info` is the FSM info plist.
* **Invariant:** The advice MUST evaluate `(or response "")` before passing it to the original function.  The upstream inserter will throw a `wrong-type-argument` crash if nil is passed.

#### 6.2.2 Media and prompt injection

* **Target:** `gptel-prompt-transform-functions`
* **Processor:** `#'macher-agent-sync-prompt-transformer`
* **Inputs received:** `(async-fn fsm)`
* **Behaviour:** During the prompt construction phase before network transmission, if `pending-media` is present in the active context, `macher-agent-sync-prompt-transformer` invokes `macher-agent-inject-pending-media`, injecting media into the payload via `gptel--inject-media` and `gptel--inject-prompt`, then clearing `pending-media`.

#### 6.2.3 Base64 virtual file system override

* **Target:** `gptel--base64-encode`
* **Interception:** `advice-add :around` (`#'macher-agent--gptel-base64-encode-advice`)
* **Inputs received:** `(file)` where `file` is an absolute string path or a raw base64-encoded string.
* **Behaviour:** The advice MUST check if the input matches any raw base64-encoded string in the active session's pending media.  If so, it returns it directly.  Otherwise, it MUST intercept the physical disk read.  It MUST query the VFS (`macher-agent--read-context-file`) using the path.  If VFS content exists, it MUST return the base64 encoded *virtual* string.  Only if it returns nil may the original upstream physical read execute.

#### 6.2.4 Tool execution scoping

* **Target:** `gptel-pre-tool-call-functions` (buffer-local hook)
* **Attached function:** `macher-agent--enforce-tool-scope`
* **Inputs received:** `(tool &rest _args)` where `tool` is a `gptel-tool` struct or string name.
* **Invariant:** The function MUST verify the tool exists exclusively within the active FSM payload snapshot, converting both the incoming tool and the authorised tool names to canonical strings using `macher-agent-canonical-tool-name`.  It MUST ignore buffer-local tool variables to avoid race conditions.
* **Output:** To allow execution, it MUST return nil.  To block execution, it MUST return a strict property list: `(:block "ERROR: [reason]")`.

#### 6.2.5 Transmit and lifecycle callbacks

* **Target:** `gptel-prompt-transform-functions` and `gptel-post-response-functions`
* **Behaviour:** When `macher-agent-gptel-transmit` is called, it MUST dynamically bind closures to these hooks to capture the FSM and execution exit codes.
* *Prompt transform input:* `(async-fn fsm)`.  The hook MUST operate strictly within the temporary transmission buffer.  It MUST read the original buffer state, parse and strip tags ephemerally within the temporary buffer, and invoke the pure composition engine (`macher-agent-compose-payload`).  Finally, it MUST apply the composed property list locally to the temporary buffer before transmission, leaving the original source buffer strictly immutable.
* *Post response input:* `(beg end)`.  The integers defining the buffer region.  The hook MUST extract the string from these bounds to trigger `macher-agent-delegate-response` callbacks.

#### 6.2.6 State restoration

* **Target:** `gptel--restore-state`
* **Interception:** `advice-add :around` (`#'macher-agent--gptel-restore-advice`)
* **Behaviour:** Prevents uncoordinated state restoration unless `macher-agent--allow-gptel-restore` is set. Initialises workspace state and active skills upon restoration.

---

### 6.3 The macher boundary for workspace and patch generation

Because `macher` relies on Emacs physical buffers and `macher-agent` uses pure-memory VFS states, the bridge MUST orchestrate a double-pass context-routing mechanism.

#### 6.3.1 Safe workspace hashing

* **Target:** `macher--workspace-hash`
* **Interception:** `advice-add :override` (`#'macher-agent--safe-workspace-hash`)
* **Inputs received:** `(workspace &rest _args)` where `workspace` could be a `macher-agent-workspace` record, a cons cell `(agent . [record])`, a cons cell `(project . "/path")`, or a string.
* **Invariant:** The original upstream function passes recursive records into Emacs' native `md5`, which causes Lisp nesting-depth crashes.  The override MUST extract the string path from the varying workspace formats and return `(md5 path)`.  It MUST NOT call the original function.

#### 6.3.2 The double-pass patch generation

* **Target:** `macher-process-request-function`
* **Processor:** `#'macher-agent-process-request`
* **Inputs received:** `(&optional status context fsm)`
* **Behaviour and invariants:**
  1. **Partitioning:** The function MUST partition active workspace edits into standard physical and virtual contexts using `macher-agent-prepare-upstream-payloads`.
  2. **Delegate formatting:** Instead of using shadow buffers, the function delegates formatting back to the native `macher` engine by calling `macher--build-patch` with each partitioned context.
  3. **Unique suffixes:** It extracts the original workspace buffer using `macher-patch-buffer` and dynamically applies a physical (`*macher-physical-patch...*`) or virtual (`*macher-virtual-patch...*`) buffer name prefix while retaining unique workspace suffixes.

---

## 7. High-level upstream bridge behaviour

### 7.1 The gptel bridge

1. **Nil response protection**: `gptel--insert-response` and `gptel-curl--stream-insert-response` are advised to gracefully handle empty (nil) streaming chunks without throwing Emacs type errors.
2. **Media injection**: Handled during state machine transitions (`gptel--fsm-transition`) and prompt construction via `gptel-prompt-transform-functions` in `macher-agent-sync-prompt-transformer`. If the context contains `pending-media`, raw base64 data is injected directly into the prompt payload before transmission and cleared from the queue.
3. **Base64 override**: The native `gptel--base64-encode` is intercepted to check the VFS first.  If the file exists in the agent's memory, it encodes the virtual string instead of reading the physical disk.
4. **State restoration**: `gptel--restore-state` is advised to coordinate session initialisation.

### 7.2 The macher bridge for patch generation

Because `macher` expects separate updates and `macher-agent` uses both physical disk files and memory-based virtual buffers, the bridge orchestrates a double-pass patch generation sequence:

1. **Context splitting:** The context preprocessor `macher-agent-prepare-upstream-payloads` partitions active workspace edits into standard physical and virtual contexts.
2. **Double-pass routing:** `macher-agent-process-request` delegates the formatting of each partitioned context back to the native `macher` engine via `macher--build-patch`.
3. **Buffer separation:** Unique, isolated buffers are produced for physical and virtual updates with preserved workspace hashes and suffixes.
