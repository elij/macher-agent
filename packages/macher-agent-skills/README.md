# macher-agent-skills

Default skills for `macher-agent`. 

Povides basic subagents, Emacs Lisp tools, and prompts required for common agentic tasks, such as planning, executing code, and reading or modifying workspace buffers. It is designed as a standalone pack that registers itself with the core agent engine.

### Installation

Because this package relies on the core agent's public API for registration, it must be loaded after `macher-agent`.

If you are using `use-package`, your configuration will look like this:

```elisp
(use-package macher-agent-skills
  :after macher-agent
  :demand t)
```

Once loaded, the package will automatically locate its internal directory and register the default skills into the core agent's user interface and orchestration registry.

### Structure

Skills are defined using simple Markdown files. If you explore the source code of this package, you will see folders for different agents (like the planner or the worker). Inside each folder is a `SKILL.md` file containing the frontmatter (defining allowed tools) and the system prompt body.

* `scripts/`: Contains the Emacs Lisp functions that power the tools.
* `macher-agent-plan/`: The simple orchestration skill used to break down user requests.
* `macher-agent-worker/`: The default general subagent skill used to execute code and modify buffers.
* `macher-agent-router/`: Implementation of [Ratchet](https://arxiv.org/abs/2605.22148) and generally more reliable than typical planner/work topologies.
* `macher-agent-synthesiser/`: Paired with `macher-agent-router`.

### Building Your Own

This package serves as a perfect template for building your own custom skill packs. You can create a new repository containing just a folder of `SKILL.md` files and a single Elisp file that calls `(macher-agent-api-register-skills-in-directory ...)` to integrate your own agents.
