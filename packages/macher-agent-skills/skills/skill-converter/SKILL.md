---
name: "skill-converter"
description: "Convert a Claude-style skill into a macher-agent compatible skill. Translates instructions, extracts bundled scripts into Emacs Lisp tools, and updates the frontmatter format."
allowed-tools:
  - read_file_in_workspace
  - list_directory_in_workspace
  - search_in_workspace
  - edit_file_in_workspace
  - multi_edit_file_in_workspace
  - write_file_in_workspace
  - move_file_in_workspace
  - delete_file_in_workspace
  - read_buffer_in_workspace
  - read_media_in_workspace
  - list_buffers_in_workspace
  - multi_edit_buffer_in_workspace
  - write_buffer_in_workspace
---
# Skill Converter

Your objective is to convert an existing Claude-style skill into a macher-agent compatible skill.

## Differences between Claude and macher-agent skills

1. **Frontmatter:**
   - Claude uses `name` and `description`.
   - macher-agent uses `name`, `description`, `allowed-tools` (a YAML list of tool names the skill has access to), and optionally `model` (to specify the LLM to use, e.g., `model: claude-3-5-sonnet-20241022`).

2. **Tools and Scripts:**
   - Claude skills often bundle standalone Python or bash scripts in a `scripts/` directory and instruct the model to run them via shell commands.
   - macher-agent provides tools as Emacs Lisp files in `skills/scripts/<tool_name>.el` or as built-in emacs tools. The orchestrator maps the names in `allowed-tools` to these ELisp functions. To convert scripts, you must rewrite the Claude bash/python scripts into `.el` scripts.
   - Use `macher-agent-make-tool` for tools that need access to the file system (this macro automatically syncs the agent's virtual edits into a physical sandbox before executing shell commands). Use `gptel-make-tool` for pure Emacs operations.

### Tool Creation Examples

#### File System Tool (`macher-agent-make-tool`)
For tools executing shell scripts against the workspace, use `macher-agent-make-tool`. It supports `:command-fn`, `:output-filter`, and `:success-fn`:

```elisp
(macher-agent-make-tool macher-agent-cargo-test-tool
  "Run 'cargo test' to test the project."
  :category "rust"
  :args nil
  :command-fn (lambda (_payload)
                (make-macher-agent-process-response
                 :payload "rtk cargo test </dev/null 2>&1"))
  :success-fn (lambda (output)
                (concat "SUCCESS: The tests ran perfectly with no errors.\n\n=== TEST OUTPUT ===\n" output)))
```

#### Pure Emacs Tool
For tools interacting with Emacs buffers directly without running shell commands on the physical filesystem:

```elisp
(macher-agent-make-tool macher-agent-get-current-datetime-tool
  "Fetch the current system date and time. Use this when you need to know the current date, time, or day of the week to answer a time-sensitive query."
  :category "system"
  :args nil
  :command-fn (lambda (_payload)
                (make-macher-agent-lisp-result-response
                 :payload (format-time-string "%A, %d %B %Y, %T %Z"))))
```

3. **Execution Model:**
   - Claude skills often just augment a general conversation.
   - macher-agent skills are explicitly loaded as presets or spawned as distinct sub-agents (e.g., `macher-agent-plan` spawning `macher-agent-worker` via `spawn_subagent`).

## Conversion Workflow

1. **Analyse the Claude Skill:**
   - Read the Claude `SKILL.md`. Extract its core role, workflow, and instructions.
   - Identify any bundled scripts it relies on.

2. **Translate Scripts to Tools:**
   - If the Claude skill has bundled scripts, rewrite them as standalone Emacs Lisp tools using the macher-agent script format (e.g., `(macher-agent-make-tool :name ... :command-fn ...)` for shell scripts, or `gptel-make-tool` for pure Emacs operations).
   - Save these tools in the `skills/scripts/` directory.

3. **Draft the macher-agent SKILL.md:**
   - Create a YAML frontmatter block with `name`, `description`, and `allowed-tools` containing the new tools and any standard workspace tools needed (like `read_file_in_workspace`, `write_buffer_in_workspace`).
   - Rewrite the body instructions. Remove references to running Python scripts or shell commands. Instead, instruct the agent to call the specific `allowed-tools` you provided.
   - Ensure the instructions dictate a clear role, critical directives, and a structured execution workflow.

4. **Output:**
   - Place the converted `SKILL.md` in `skills/<new-skill-name>/SKILL.md`.
