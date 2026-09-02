<h1 align="center">macher-agent</h1>

https://github.com/user-attachments/assets/35908782-ee2b-4243-8b93-ad8381cfee5c

The `macher-agent` package provides an Emacs-native artificial intelligence agent harness. It integrates deeply with `gptel` and `macher` to enable autonomous, multi-agent workflows directly within Emacs buffers.

The architecture operates entirely inside native Emacs buffers rather than external terminal interfaces. Tools and subagents operate as sentinels within dedicated buffers. Subagents coordinate through direct Emacs Lisp callbacks for point-to-point communication. Programmatic tool calling runs within a secure Emacs Lisp sandbox, allowing agents to chain operations and transform data in a single turn. Tools use `gptel-make-tool` and `macher-agent-with-presentation-context`, returning structured data first and presentation second, using Emacs as an extensible multiplexing environment.

## Subagent approach

The harness is optimised to execute large numbers of concurrent agents inside a single Emacs session. Each subagent operates on an isolated Virtual File System (VFS) context cloned from its parent. The Virtual File System and the sandboxed evaluation runtime allow agents to resolve complex tasks without premature disk modification. Proposed file and buffer modifications are staged in memory and presented as unified diffs for review before applying changes to disk.

## Integration with gptel

The package integrates with `gptel` across several core boundaries:

- Functions registered in `gptel-prompt-transform-functions` synchronise the Virtual File System, dynamically resolve inline skill mentions (such as `@skill-name`), compile hidden prompt directives, and prune duplicate tool call history prior to transmission.
- Finite-state machine handlers (`gptel-fsm-handlers`) capture user prompts, inject base64-encoded visual media, protect callback closures, and trigger task completion flushes when execution reaches terminal states.
- The pre-tool execution hook `gptel-pre-tool-call-functions` enforces strict tool scoping, blocking invocations of tools not permitted in the active skill configuration.
- Settings such as `gptel-model`, `gptel-backend`, `gptel-system-prompt`, and `gptel-tools` are scoped buffer-locally to allow each agent buffer to maintain an independent identity.

## Integration with macher

The package extends `macher` to provide workspace-level isolation and Virtual File System capabilities:

- `macher` context persists until invalidated (clear, merge, or fail-fast out-of-band modification).
- `macher` tools are wrapped to inject a persistent context. This prevents tool calls from executing against unhydrated contexts and ensures disk-based operations operate on in-memory buffers.
- File modifications and buffer edits stage directly in the Virtual File System context. Diffs are generated separately for buffer changes and file modifications, presenting unified diffs for user review before committing changes.
- Active workspaces and persistent contexts register in `macher-agent-active-workspaces` by project root. Subagents receive isolated child contexts that merge back into the orchestrator context upon task submission.
- The tool `search_in_workspace` uses direct file system traversal to ensure consistent performance and avoid garbage collection bottlenecks on large workspaces.

## Plug-in model and pipeline registry

The architecture uses an extensible plug-in model built on ordered pipeline reducers and lifecycle hooks. Custom packages and user configurations extend core behaviour by registering steps into named pipelines with explicit priority depths.

### Pipeline architecture

Pipelines process state property lists sequentially through registered step functions. Steps execute in ascending priority order, where lower integer values execute earlier in the pipeline sequence.

Core pipeline registries include:

- `context-resolution`: Resolves the active `macher-context` from inputs, buffers, payloads, state machines, or workspace directories.
- `preset-composition`: Merges skill definitions, allowed tools, model parameters, and programmatic tool calling primitives into unified payloads.
- `transmission`: Hydrates model settings, compiles hidden directives, injects memory tools, and prepares the network payload for `gptel-send`.
- `artifact-compose`: Packages completed subagent outputs, diffs, and context modifications when a task finishes.
- `payload-merge`: Merges subagent Virtual File System diffs into the parent orchestrator context.

### Registering pipeline steps

Use `macher-agent-register-pipeline-step` to attach custom logic to a pipeline. The function accepts the pipeline symbol, the step function, and an integer priority.

```elisp
;; Define a custom transmission step that injects a project header directive
(defun my-custom-transmission-header-step (state orig-buf presets skills redirect)
  "Inject a workspace banner into the transmission directives."
  (let ((banner (format "PROJECT ENVIRONMENT: %s" (macher-agent-root default-directory))))
    (push banner (macher-agent-transmission-state-directives state)))
  state)

;; Register the step in the transmission pipeline at priority 65
(macher-agent-register-pipeline-step
 'transmission
 #'my-custom-transmission-header-step
 65)

;; Define a custom context resolution step for custom buffer types
(defun my-custom-buffer-context-step (state)
  "Resolve context from specialised buffer-local properties."
  (if (and (plist-get state :resolved) (null (plist-get state :input)))
      state
    (let ((input (plist-get state :input)))
      (if (and (bufferp input) (buffer-local-value 'my-custom-context-var input))
          (plist-put state :resolved (buffer-local-value 'my-custom-context-var input))
        state))))

;; Register the custom context resolver at priority 12
(macher-agent-register-pipeline-step
 'context-resolution
 #'my-custom-buffer-context-step
 12)
```

### Lifecycle hooks

The framework provides event hooks to monitor task flushes and workspace changes:

- `macher-agent-task-flush-hook`: Runs when a task completes and flushes context data.
- `macher-agent-vfs-flush-hook`: Runs after the Virtual File System processes file and buffer modifications.
- `macher-agent-context-mutated-hook`: Runs whenever the Virtual File System context is modified.

```elisp
;; Example: Hook into task flushes to log completed interactions
(defun my-task-flush-logger (context)
  "Log task flush events for CONTEXT."
  (let ((root (and context (macher-agent-context-project-root context))))
    (message "Task completed for workspace: %s" root)))

(add-hook 'macher-agent-task-flush-hook #'my-task-flush-logger)

;; Example: Listen for Virtual File System mutations
(defun my-vfs-mutation-listener (path)
  "React to changes staged at PATH."
  (message "Workspace path modified in VFS: %s" path))

(add-hook 'macher-agent-context-mutated-hook #'my-vfs-mutation-listener)
```

## Agent-to-agent communication

Agents interact through point-to-point Agent-to-Agent (A2A) payloads and callback closures.

| Tool | Description | Communication type | Virtual File System synchronisation |
| --- | --- | --- | --- |
| `delegate_tasks_to_subagents` | Dispatches tasks synchronously to worker agents and aggregates responses | Direct message dispatch | Merges child diffs upon task submission |
| `execute_subagents` | Dispatches fire-and-forget background tasks in parallel | Asynchronous dispatch | Staged in child context |
| `submit_task_result` | Submits completed task output back to the originating caller | Artefact update | Merges Virtual File System diffs |
| `spawn_subagent` | Creates a named subagent buffer configured with specific skill presets | Lifecycle initialisation | Clones parent context |
| `send_message` | Sends an asynchronous message to a resident specialist bot buffer | Direct point-to-point message | Transmits instructions directly |
| `wait_for_message` | Suspends a resident specialist bot until an incoming message arrives | Event suspension | Merges VFS from sender |
| `wait_for_vfs_semaphore` | Suspends execution until a Virtual File System resource is created or modified | Change notification listener | Synchronises targeted resource path |

## Examples

### Zero-Mem benchmarks (100,000 tokens)

Memory recall beyond the `macher-agent-max-context-chars` boundary operates through the `search_conversation_history` tool, to provide a continuous memory. Subagents are also able to access the memory of the originating context using `search_parent_conversation_history`.

1,000 Traces

|Engine         |Time      |GC Cycles|GC Time   |
|---------------|----------|---------|----------|
|Float PPR      |0.050142 s|2        |0.034173 s|
|Fixed-Point PPR|0.028505 s|1        |0.016375 s|
|Glob           |0.000026 s|0        |0 s       |

5,000 Traces

|Engine         |Time      |GC Cycles|GC Time   |
|---------------|----------|---------|----------|
|Float PPR      |0.277900 s|11       |0.200374 s|
|Fixed-Point PPR|0.142280 s|5        |0.092353 s|
|Glob           |0.000062 s|0        |0 s       |

10,000 Traces

|Engine         |Time      |GC Cycles|GC Time   |
|---------------|----------|---------|----------|
|Float PPR      |0.589837 s|20       |0.429688 s|
|Fixed-Point PPR|0.293908 s|9        |0.197646 s|
|Glob           |0.000115 s|0        |0 s       |

### Programmatic tool calling (PTC) for token efficiency

Programmatic tool calling in Emacs Lisp allows complex operations that would otherwise require multiple conversational turns to execute within a single request. The execution runs inside a yielding Emacs Lisp sandbox.

<img alt="PTC" src="https://github.com/user-attachments/assets/299b08c8-0d22-46e9-acee-fc2b631c5d30" />

The tools `list-directory-in-workspace`, `spawn-subagent`, and `delegate-tasks-to-subagents` are exposed as callable primitives:

```elisp
(let*
    ((listing (list-directory-in-workspace "" "" ""))
     (lines (split-string listing "\n"))
     (file-lines
      (cl-loop
       for line in lines when (string-prefix-p "file: " line) collect (substring line 6)))
     (agent-count (length file-lines))
     (agent-names (cl-loop for i from 0 below agent-count collect (format "agent-%d" i)))
     (tasks
      (cl-loop
       for path in file-lines for name in agent-names
       collect (list :buffer_name name
                     :instructions (format "Read the first paragraph of the file at '%s' and provide a concise summary." path)
                     :presets (list "macher-agent-worker")))))
  (if (zerop agent-count)
      "No files found"
    (progn
      (mapcar (lambda (name) (spawn-subagent name (list "macher-agent-worker"))) agent-names)
      (delegate-tasks-to-subagents tasks))))
```

### Multi-agent Virtual File System

Each subagent operates on a discrete Virtual File System context within the workspace, featuring automatic conflict detection and merge resolution.

<img alt="VFS" src="https://github.com/user-attachments/assets/123a9010-e39c-4e59-967c-a032a71a52cc" />

## Installation

### Prerequisites

Ensure the following utilities and packages are available:

- Emacs 30.1 or higher
- `gptel` and `macher`
- Git
- Rsync

### Package configuration

Install and configure `macher-agent` using `use-package`:

```elisp
(use-package macher-agent
  :vc (:url "https://github.com/elij/macher-agent/")
  :ensure t
  :after (gptel macher)
  :hook (gptel-mode . macher-agent-mode)
  :config
  (require 'macher-agent-vfs)      ;; Enable Virtual File System
  (require 'macher-agent-sandbox)  ;; Enable Programmatic Tool Calling
  (require 'macher-agent-zero-mem) ;; Enable dual-ledger conversation memory
  (macher-agent-install))
```

## Getting started

1. Initialise a Git repository in your project directory:

```bash
git init
```

2. Open or create a file within the project, then start an interactive chat buffer using standard `gptel` or `macher-discuss`.
3. Interact with the language model directly. The model can inspect files, stage changes in the Virtual File System, evaluate scripts using programmatic tool calling, and dispatch subagents.

## Documentation and wiki

For detailed guides, architectural diagrams, and cookbook patterns, refer to the [project wiki](https://github.com/elij/macher-agent/wiki).
