;;; macher-agent-core.el --- Core state for Macher Agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Core state variables and functions for Macher Agent.

;;; Code:

(require 'cl-lib)

;; Buffer-local agent state
(defvar-local macher-agent-parent-buffer nil
  "Store name of parent chat buffer from which current chat branched.

This buffer-local variable retains the string name of the originating parent
buffer when a chat is branched.

Return the parent buffer name string or nil.

Side effects: None.")

(defvar-local macher-agent--active-skill-sym nil
  "Store symbol representing the currently active skill preset in buffer.

This buffer-local variable holds the active skill symbol bound to the current
chat buffer.

Return the active skill symbol or nil.

Side effects: None.")

(defvar-local macher-agent--active-ptc-primitives nil
  "Store active Programmatic Tool Calling primitives for current buffer.

Hold a list of primitive tool symbols or names active in the buffer-local
execution context.

Return list of active primitive symbols or names.
Side effects: Buffer-local variable.")

(defvar-local macher-agent--pending-instructions-queue nil
  "Queue of ephemeral thoughts and instructions to inject on the next turn.

Return list of pending instruction strings, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--final-result nil
  "Store the final result string or data returned by agent execution.")

(defvar-local macher-agent--persistent-context nil
  "Store the buffer-local persistent VFS context structure.

Hold the `macher-context' instance bound to the current buffer across
agent turns.

Return the `macher-context' struct, or nil if unset.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--current-task-id nil
  "Store task ID for current sub-agent execution.")

(defvar-local macher-agent--is-subagent nil
  "Flag whether the current buffer is managed as a sub-agent.

Non-nil indicates that the buffer is an isolated sub-agent buffer.

Return non-nil if current buffer is a sub-agent, otherwise nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--ready-to-reap nil
  "Flag whether the sub-agent buffer is ready for garbage collection.

Non-nil indicates that the sub-agent task has completed and can be reaped.

Return non-nil if sub-agent buffer is ready to be reaped, otherwise nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent-presets nil
  "Store list of active preset or skill symbols for current buffer.

Holds the active preset and skill symbols configured for the current buffer.

Return list of active preset or skill symbols, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--parent-buffer nil
  "Store parent orchestrator buffer for current sub-agent.

Holds the buffer object of the parent agent that spawned this sub-agent.

Return parent buffer object, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--parent-stack nil
  "Stack of parent orchestrator frames for current sub-agent buffer.

Holds a list of parent frame plists pushed as peers or orchestrators
communicate.

Return list of parent frame plists, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--suppress-patch nil
  "Flag whether patch review generation is suppressed.

Non-nil suppresses virtual patch review generation on task completion.

Return non-nil if patch creation is suppressed, otherwise nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--boot-directive nil
  "Store initial request boot directive for sub-agent buffer.

Holds the boot directive string configured for the current buffer.

Return boot directive string, or nil.

Side effects: Buffer-local variable.")


(defvar-local macher-agent--is-workspace nil
  "Non-nil if current buffer is an agent workspace buffer.")

(defvar-local macher--workspace nil
  "The active workspace structure for current buffer.")

;; Permanent local puts
(put 'macher-agent--pending-instructions-queue 'permanent-local t)
(put 'macher-agent--current-task-id 'permanent-local t)
(put 'macher-agent--is-subagent 'permanent-local t)
(put 'macher-agent--ready-to-reap 'permanent-local t)
(put 'macher-agent-presets 'permanent-local t)
(put 'macher-agent--parent-buffer 'permanent-local t)
(put 'macher-agent--parent-stack 'permanent-local t)
(put 'macher-agent--active-ptc-primitives 'permanent-local t)
(put 'macher-agent--suppress-patch 'permanent-local t)
(put 'macher-agent--boot-directive 'permanent-local t)
(put 'macher-agent--persistent-context 'permanent-local t)


;;

(defcustom macher-agent-display-subagent-fn nil
  "Specify function to display a sub-agent buffer during execution.

BUFFER is the buffer object to display.
If nil, the buffer executes silently in the background.

Return the display function or nil.
Side effects: None."
  :type '(choice (const :tag "Silent Background Execution" nil)
                 function)
  :group 'macher-agent)

(defcustom macher-agent-hide-subagent-fn nil
  "Specify function to hide a sub-agent buffer after execution finishes.

BUFFER is the buffer object to hide.
If nil, no action is taken when finished.

Return the hide function or nil.
Side effects: None."
  :type '(choice (const :tag "Do Nothing" nil)
                 function)
  :group 'macher-agent)

;;

(defun macher-agent-ui-show (&optional buf)
  "Display the user interface for buffer BUF.

BUF is the optional buffer to display, defaulting to current buffer.

Return nil.

Side effects: Opens or focuses user interface window, invoking
`macher-agent-display-subagent-fn' when non-nil."
  (let ((target-buf (or buf (current-buffer))))
    (when macher-agent-display-subagent-fn
      (funcall macher-agent-display-subagent-fn target-buf))))




(provide 'macher-agent-core)
;;; macher-agent-core.el ends here
