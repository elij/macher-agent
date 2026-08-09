<h1 align="center">macher-agent</h1>

https://github.com/user-attachments/assets/35908782-ee2b-4243-8b93-ad8381cfee5c

The macher-agent project is a *fully* Emacs native agentic harness. At its core, it is a collection of gptel presets and integrations with macher and gptel.

A truly native Emacs harness relies on buffers rather than terminal user interfaces with captures and Elisp over slash commands (no external middleware, SDKs, ACPs etc.). Tools and subagents act as sentinels operating in their own buffers. Subagents communicate via Elisp callbacks for one-to-one interactions or through hooks for broadcasting messages. Programmatic tool calling happens within an Elisp sandbox, integrating tool calls directly with Elisp. The `macher-agent-make-tool` function returns primitive types first and presentation second, fully utilising Emacs as a customisable multiplexing environment.

## Approach to subagents

The architecture is optimised to operate large numbers of agents running concurrently within a single Emacs instance. The Virtual File System and the Elisp sandbox with programmatic tool calling allow agents to solve problems before requiring explicit permissions from the user with changes being present as a unified diff.

## Integration with/changes to gptel (tooling, presets, UI)

The integration with gptel isolates setting changes to within a workspace. It adds an advice to the encoding of base64 to inject non-file backed media, such as the media used in computer use interactions. It also implements finite-state machine tracking for workspace-local tool access.

## Integration with/changes to macher (context/VFS/patch generation)

The default context is orphaned as it is inaccessible within a capture block. All macher tools and patch generation processes are wrapped to inject a macher-agent accessible context, ensuring hydration of the Virtual File System works for disk-based tools. Macher generates diffs for both buffer changes and file changes as 2 different unified diffs. The Virtual File System is persistent until it is cleared, applied, or invalidated (by modifications outside Emacs). When subagents are spawned, they operate on a sibling Virtual File System until they are terminated.

## Memory Recall

### Macher Agent Zero-Mem Benchmarks (100k tokens)

Memory recall beyond the `gptel-max-tokens` event horizon via the `search_conversation` tool

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
