<h1 align="center">macher-agent</h1>

https://github.com/user-attachments/assets/35908782-ee2b-4243-8b93-ad8381cfee5c

The macher-agent project is a *fully* Emacs native agentic harness. At its core, it is a collection of gptel presets and integrations with macher and gptel.

A truly native Emacs harness relies on buffers rather than terminal user interfaces with captures and Elisp over slash commands (no external middleware, SDKs, ACPs etc.). Tools and subagents act as sentinels operating in their own buffers. Subagents communicate via Elisp callbacks for one-to-one interactions or through hooks for broadcasting messages. Programmatic tool calling happens within an Elisp sandbox, integrating tool calls directly with Elisp. The `macher-agent-make-tool` function returns primitive types first and presentation second, fully utilising Emacs as a customisable multiplexing environment.

## Approach to subagents

The architecture is optimised to operate large numbers of agents running concurrently within a single Emacs instance. The Virtual File System and the Elisp sandbox with programmatic tool calling allow agents to solve problems before requiring explicit permissions from the user with changes being presentws as a unified diff.

## Integration with/changes to gptel (tooling, presets, UI)

The integration with gptel isolates setting changes to within a workspace. It adds an advice to the encoding of base64 to inject non-file backed media, such as the media used in computer use interactions. It also implements finite-state machine tracking for workspace-local tool access. gptel tools are accessible within the PTC Elisp sandbox.

## Integration with/changes to macher (context/VFS/patch generation)

The default context is orphaned as it is inaccessible within a capture block. All macher tools and patch generation processes are wrapped to inject a macher-agent accessible context, ensuring hydration of the Virtual File System works for disk-based tools. Macher generates diffs for both buffer changes and file changes as 2 different unified diffs. The Virtual File System is persistent until it is cleared, applied, or invalidated (by modifications outside Emacs). When subagents are spawned, they operate on a sibling Virtual File System until they are terminated.

## Examples

### Macher Agent Zero-Mem Benchmarks (100k tokens)

Memory recall beyond the `gptel-max-tokens` event horizon via the `search_conversation_history` tool

1000 Traces

| Engine          | Time       | GC Cycles | GC Time    |
|-----------------|------------|-----------|------------|
| Float PPR       | 0.050142 s | 2         | 0.034173 s |
| Fixed-Point PPR | 0.028505 s | 1         | 0.016375 s |
| Glob            | 0.000026 s | 0         | 0          |

5000 Traces

| Engine          | Time       | GC Cycles | GC Time    |
|-----------------|------------|-----------|------------|
| Float PPR       | 0.277900 s | 11        | 0.200374 s |
| Fixed-Point PPR | 0.142280 s | 5         | 0.092353 s |
| Glob            | 0.000062 s | 0         | 0          |

10000 Traces

| Engine          | Time       | GC Cycles | GC Time    |
|-----------------|------------|-----------|------------|
| Float PPR       | 0.589837 s | 20        | 0.429688 s |
| Fixed-Point PPR | 0.293908 s | 9         | 0.197646 s |
| Glob            | 0.000115 s | 0         | 0          |

[Zero-Mem](https://arxiv.org/abs/2607.29377)

[BEAM](https://arxiv.org/pdf/2510.27246)


``` reasoning
The user is asking "what have we discussed about London?".
I notice the system alert mentions that the agent truncated 46 lines of early history.

tool (search_conversation_history :query "London" :conte ...)
(:name "search_conversation_history" :args (:query "London" :context_lines 10))

--- Match near line 15 ---
While London as a whole is a massive metropolis, the **City of London** (the historic financial district at its core) is actually a separate, tiny ceremonial city. Spanning just over one square mile, it has its own mayor, distinct governing body, and is the smallest city in the UK.

--- Match near line 13 ---
Here are 5 random and fascinating facts about London:

--- Match near line 10 ---
Another line about London that isn't useful
```


### Use of PTC for token efficiency

Programmatic tool calling in Elisp allows small scripts that would typically take many tool calling rounds to execute in a single call. PTC usses a yieding Elisp sandbox for all tool calls.

*NB: script drafted by an LLM from real world usage*

`list-directory-in-workspace`, `spawn-subagent` and  `delegate-tasks-to-subagents` are gptel tools.

<img alt="PTC" src="https://github.com/user-attachments/assets/299b08c8-0d22-46e9-acee-fc2b631c5d30" />


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
                     :instructions (format "Read the first paragraph of the file at '%s' and provide a concise one-sentence summary." path)
                     :presets (list "macher-agent-worker")))))
  (if (zerop agent-count)
      "No files found"
    (progn
      (mapcar (lambda (name) (spawn-subagent name (list "macher-agent-worker"))) agent-names)
      (delegate-tasks-to-subagents tasks))))

```
### Multi agent VFS

Each agent operates a discrete VFS (`macher` context) in the workspace with auto fail-fast merge resolution.

<img alt="VFS" src="https://github.com/user-attachments/assets/123a9010-e39c-4e59-967c-a032a71a52cc" />

### A2A communication

In `macher-agent` the princple coordinatation approach are callbacks which allows the recreation of most topologies.

Agent to agent communication tools

|Tool                         |Description |A2A type                      |VFS sync|
|-----------------------------|------------|------------------------------|--------|
|`delegate_tasks_to_subagents`|Execute     |`SEND_MESSAGE`                |Merge   |
|`submit_task_result`         |Respond.    |`ARTIFACT_UPDATE`             |Merge.  |
|`spawn_subagent`             |Instantiate.|Lifecycle / Initialisation    |Merge.  |
|Internal                     |Locks..     |`ACQUIRE_LOCK`, `RELEASE_LOCK`|P2P.    |
|`wait_for_vfs_semaphore`     |Block.      |`ACQUIRE_LOCK`<br>            |P2P.    |
|`send_message`               |Message.    |`SEND_MESSAGE`                |Merge.  |

## Installation

### Dependencies

Ensure the following utilities and packages are installed

* Emacs 29.1 or higher
* `gptel` and `macher` packages
* git
* rsync

You can install and configure the package using `use-package`.

```elisp
(use-package macher-agent
  :vc (:url "https://github.com/elij/macher-agent/")
  :ensure t
  :after (gptel macher))

```

## Getting started

To begin using macher-agent in a project, initialise a git repository.

```bash
git init

```

Next, capture a discussion buffer using `macher-discuss` or standard `gptel`. From within this buffer, you can interact with the language model and spin up agents to handle workspace tasks.

## Documentation and wiki

The macher-agent framework is highly modular. For comprehensive guides, command mapping matrices, and advanced cookbook examples, please refer to the [project wiki](https://github.com/elij/macher-agent/wiki).
