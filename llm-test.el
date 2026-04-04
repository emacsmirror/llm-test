;;; llm-test.el --- LLM-driven testing for packages -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/llm-test
;; Package-Requires: ((emacs "29.1") (llm "0.18.0") (yaml "0.5.0") (futur "1.2"))
;; Keywords: testing, tools
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; llm-test is a testing library that uses LLM agents to interpret
;; natural-language test specifications (written in YAML) and execute them
;; against a fresh Emacs process.  Tests are registered as ERT tests, so they
;; integrate with the standard Emacs test infrastructure.
;;
;; Usage:
;;   (require 'llm-test)
;;   (setq llm-test-provider (make-llm-openai :key "..."))
;;   ;; or set LLM_TEST_PROVIDER_ELISP in the environment
;;   (llm-test-register-tests "path/to/testscripts/")

;;; Code:

(require 'llm)
(require 'yaml)
(require 'llm-openai nil t)
(require 'llm-claude nil t)
(require 'llm-gemini nil t)
(require 'llm-vertex nil t)
(require 'llm-ollama nil t)

(require 'ert)
(require 'cl-lib)
(require 'futur)

(defgroup llm-test nil
  "LLM-driven testing for Emacs packages."
  :group 'tools)
(defcustom llm-test-emacs-executable "emacs"
  "Path to the Emacs executable used to run tests.
A fresh Emacs process (`emacs -Q') is launched for each test."
  :type 'string
  :group 'llm-test)

(defcustom llm-test-provider nil
  "The LLM provider to use for the test agent.
This should be an object created by one of the `make-llm-*' constructors
from the `llm' package."
  :type 'sexp
  :group 'llm-test)

(defcustom llm-test-provider-elisp-environment-variable
  "LLM_TEST_PROVIDER_ELISP"
  "Environment variable containing Elisp to construct a provider.
When `llm-test-provider' and `:provider' are both nil, `llm-test'
evaluates the value of this environment variable and uses the result as
the provider.  The Elisp is trusted and should construct an LLM provider
object, typically with a single `progn' form."
  :type 'string
  :group 'llm-test)

(defun llm-test--provider-from-elisp (provider-elisp)
  "Evaluate PROVIDER-ELISP and return the resulting provider."
  (let* ((trimmed (string-trim provider-elisp))
         (read-result (read-from-string trimmed))
         (form (car read-result))
         (position (cdr read-result)))
    (unless (string-match-p "\\`[[:space:]]*\\'"
                            (substring trimmed position))
      (error "Provider Elisp contained trailing data"))
    (let ((provider (eval form t)))
      (unless provider
        (error "Provider Elisp evaluated to nil"))
      provider)))

(defun llm-test--provider-from-environment ()
  "Return a provider constructed from the environment, or nil if unset."
  (let ((provider-elisp
         (getenv llm-test-provider-elisp-environment-variable)))
    (when (and provider-elisp
               (not (string-match-p "\\`[[:space:]]*\\'" provider-elisp)))
      (condition-case err
          (llm-test--provider-from-elisp provider-elisp)
        (error
         (error "Failed to evaluate %s: %s"
                llm-test-provider-elisp-environment-variable
                (error-message-string err)))))))

(defun llm-test--resolve-provider (&optional provider)
  "Resolve PROVIDER, falling back to configuration and the environment.
Explicit PROVIDER takes precedence."
  (or provider
      llm-test-provider
      (llm-test--provider-from-environment)
      (error
       "No LLM provider configured.  Set `llm-test-provider', pass :provider, or set %s"
       llm-test-provider-elisp-environment-variable)))

(defcustom llm-test-frame-width 80
  "Width in columns of the frame created in the test Emacs process."
  :type 'integer
  :group 'llm-test)

(defcustom llm-test-frame-height 40
  "Height in lines of the frame created in the test Emacs process."
  :type 'integer
  :group 'llm-test)

(defcustom llm-test-warnings-as-errors nil
  "When non-nil, any improvement suggestions cause the test to fail."
  :type 'boolean
  :group 'llm-test)

;;; Data Structures

(cl-defstruct llm-test-spec
  "A single test specification."
  description)

(cl-defstruct llm-test-group
  "A group of tests with shared setup."
  name setup tests)

;;; YAML Parsing

(defun llm-test--parse-yaml-string (yaml-string)
  "Parse YAML-STRING into a list of `llm-test-group' structs.
The YAML should contain a single group document with keys:
  group: <name>
  setup: <natural language setup description>
  tests:
    - description: <test description>"
  (let* ((parsed (yaml-parse-string yaml-string))
         (group-name (gethash 'group parsed))
         (setup (gethash 'setup parsed))
         (tests-array (gethash 'tests parsed))
         (tests (mapcar (lambda (test-hash)
                          (make-llm-test-spec
                           :description (gethash 'description test-hash)))
                        (append tests-array nil))))
    (unless group-name
      (error "YAML test spec missing required 'group' key"))
    (unless tests
      (error "YAML test spec missing required 'tests' key"))
    (make-llm-test-group
     :name group-name
     :setup (or setup "")
     :tests tests)))

(defun llm-test--parse-yaml-file (file)
  "Parse a YAML test FILE into a `llm-test-group' struct."
  (llm-test--parse-yaml-string
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string))))

;;; Emacs Subprocess Control

(defcustom llm-test-timeout 30
  "Timeout in seconds for evaluating expressions in the test Emacs process."
  :type 'integer
  :group 'llm-test)

(defvar llm-test--server-name-counter 0
  "Counter for generating unique server names.")

(cl-defun llm-test--start-emacs (&key extra-load-path init-forms)
  "Start a fresh Emacs process for testing.
EXTRA-LOAD-PATH is a list of directories to add to the subprocess
`load-path'.

INIT-FORMS is a list of elisp forms to evaluate at startup.  Returns a
plist with :process, :server-name, :socket-dir, and :init-file."
  (let* ((server-name (format "llm-test-%d-%d"
                              (emacs-pid)
                              (cl-incf llm-test--server-name-counter)))
         (socket-dir (make-temp-file "llm-test-socket-" t))
         (init-file (make-temp-file "llm-test-init-" nil ".el"))
         (_ (with-temp-file init-file
              (insert (format "(setq server-socket-dir %S server-name %S)\n"
                              socket-dir server-name))
              (dolist (dir extra-load-path)
                (insert (format "(add-to-list 'load-path %S)\n" dir)))
              (dolist (form init-forms)
                (insert (format "%S\n" form)))))
         (process (start-process
                   (format "llm-test-emacs-%s" server-name)
                   (format " *llm-test-emacs-%s*" server-name)
                   llm-test-emacs-executable
                   "-Q"
                   "-l" init-file
                   (format "--daemon=%s" server-name))))
    ;; Wait for the daemon to be ready by polling for the socket file.
    (let ((deadline (+ (float-time) llm-test-timeout))
          (socket-file (expand-file-name server-name socket-dir)))
      (while (and (< (float-time) deadline)
                  (not (file-exists-p socket-file)))
        (sit-for 0.1))
      (unless (file-exists-p socket-file)
        (when (process-live-p process)
          (kill-process process))
        (error "Timed out waiting for test Emacs daemon to start")))
    ;; Set the frame dimensions so that window-based visibility queries
    ;; return meaningful results in the daemon.
    (let ((frame-info (list :process process
                            :server-name server-name
                            :socket-dir socket-dir
                            :init-file init-file)))
      (llm-test--eval-in-emacs
       frame-info
       (format "(set-frame-size (selected-frame) %d %d)"
               llm-test-frame-width llm-test-frame-height))
      frame-info)))

(defun llm-test--eval-in-emacs (emacs-info sexp)
  "Evaluate SEXP in the test Emacs process described by EMACS-INFO.
SEXP should be a string of elisp to evaluate.
Returns the result as a string.  Times out after `llm-test-timeout' seconds.
Uses `call-process' to avoid processing other async events (like LLM
API responses) while waiting, which would cause re-entrant callbacks."
  (let* ((server-name (plist-get emacs-info :server-name))
         (socket-dir (plist-get emacs-info :socket-dir)))
    (with-temp-buffer
      (let ((exit-code
             (call-process "emacsclient" nil t nil
                           (format "--socket-name=%s"
                                   (expand-file-name server-name socket-dir))
                           "--eval" sexp)))
        (if (= exit-code 0)
            (string-trim (buffer-string))
          (error "Emacsclient eval failed (exit %d): %s"
                 exit-code (buffer-string)))))))

(defun llm-test--eval-in-emacs-async (emacs-info sexp)
  "Evaluate SEXP in the test Emacs process described by EMACS-INFO.
SEXP should be a string of elisp to evaluate.
Returns a `futur' that resolves to the result string, or signals an
error if emacsclient fails.  Unlike `llm-test--eval-in-emacs', this
does not block the Emacs event loop."
  (let* ((server-name (plist-get emacs-info :server-name))
         (socket-dir (plist-get emacs-info :socket-dir))
         (buf (generate-new-buffer " *llm-test-eval*" t)))
    (futur-let*
        ((exit-code <- (futur-process-call
                        "emacsclient" nil buf nil
                        (format "--socket-name=%s"
                                (expand-file-name server-name socket-dir))
                        "--eval" sexp))
         (output (with-current-buffer buf
                   (prog1 (string-trim (buffer-string))
                     (kill-buffer buf)))))
      (if (= exit-code 0)
          (futur-done output)
        (futur-failed
         (list 'error
               (format "Emacsclient eval failed (exit %d): %s"
                       exit-code output)))))))

(defun llm-test--stop-emacs (emacs-info)
  "Stop the test Emacs process described by EMACS-INFO."
  (let* ((server-name (plist-get emacs-info :server-name))
         (socket-dir (plist-get emacs-info :socket-dir))
         (init-file (plist-get emacs-info :init-file))
         (process (plist-get emacs-info :process)))
    (ignore-errors
      (call-process "emacsclient" nil nil nil
                    (format "--socket-name=%s"
                            (expand-file-name server-name socket-dir))
                    "--eval" "(kill-emacs)"))
    (when (process-live-p process)
      (delete-process process))
    (ignore-errors
      (delete-directory socket-dir t))
    (when init-file
      (ignore-errors
        (delete-file init-file)))))

;;; Agent Tools and Loop

(defcustom llm-test-max-iterations 100
  "Maximum number of agent iterations before forcing a timeout failure."
  :type 'integer
  :group 'llm-test)

(defcustom llm-test-debug nil
  "When non-nil, log each tool call and its result to *Messages*."
  :type 'boolean
  :group 'llm-test)

(defcustom llm-test-max-tool-result-length 2000
  "Maximum character length for a tool result returned to the LLM.
Results longer than this are truncated with a notice.  This prevents
large return values (e.g. struct representations) from bloating the
conversation context and causing API timeouts."
  :type 'integer
  :group 'llm-test)

(defconst llm-test--frame-state-elisp
  "(progn
     (require 'cl-lib)
     (require 'json)
     (redisplay t)
     (cl-labels
         ((llm-test--plain-string (value)
            (cond
             ((stringp value) (substring-no-properties value))
             ((and (consp value) (stringp (car value)))
              (substring-no-properties (car value)))
             (t nil)))
          (llm-test--display-replacement (value)
            (cond
             ((null value) nil)
             ((llm-test--plain-string value))
             (t \"[display]\")))
          (llm-test--add-event (table pos string)
            (when (and string pos)
              (puthash pos (append (gethash pos table) (list string)) table)))
          (llm-test--append-events (table pos pieces)
            (dolist (string (gethash pos table))
              (push string pieces))
            pieces)
          (llm-test--display-state-at (pos end)
            (let* ((display-data (get-char-property-and-overlay pos 'display))
                   (overlay (cdr display-data))
                   (display (llm-test--display-replacement
                             (car display-data)))
                   (display-end
                    (if overlay
                        (overlay-end overlay)
                      (next-single-char-property-change
                       pos 'display nil end))))
              (when display
                (list display overlay display-end))))
          (llm-test--next-boundary (pos end)
            (min end
                 (next-overlay-change pos)
                 (next-single-char-property-change pos 'display nil end)))
          (llm-test--next-display-change (pos end active-state)
            (let ((next (llm-test--next-boundary pos end)))
              (while (and (< next end)
                          (equal (llm-test--display-state-at next end)
                                 active-state))
                (setq next (llm-test--next-boundary next end)))
              next))
          (llm-test--window-contents (w)
            (with-current-buffer (window-buffer w)
              (let* ((start (window-start w))
                     (end (window-end w t))
                     (overlays (cl-remove-duplicates
                                (append (overlays-in start end)
                                        (overlays-at start)
                                        (overlays-at end))
                                :test #'eq))
                     (before-table (make-hash-table :test #'eql))
                     (after-table (make-hash-table :test #'eql))
                     (pieces nil)
                     (pos start))
                (dolist (ov overlays)
                  (let* ((ov-start (overlay-start ov))
                         (ov-end (overlay-end ov))
                         (before
                          (llm-test--plain-string
                           (overlay-get ov 'before-string)))
                         (after
                          (llm-test--plain-string
                           (overlay-get ov 'after-string)))
                         (display
                          (llm-test--display-replacement
                           (overlay-get ov 'display))))
                    (when (and before ov-start
                               (<= start ov-start) (<= ov-start end))
                      (llm-test--add-event before-table ov-start before))
                    (when (and display ov-start ov-end (= ov-start ov-end)
                               (<= start ov-start) (<= ov-start end))
                      (llm-test--add-event before-table ov-start display))
                    (when (and after ov-end
                               (<= start ov-end) (<= ov-end end))
                      (llm-test--add-event
                       (if (and ov-start (= ov-start ov-end))
                           before-table
                         after-table)
                       ov-end
                       after))))
                (while (< pos end)
                  (setq pieces
                        (llm-test--append-events before-table pos pieces))
                  (let ((display-state
                         (llm-test--display-state-at pos end)))
                    (if display-state
                        (let* ((display (car display-state))
                               (next-pos
                                (llm-test--next-display-change
                                 pos end display-state)))
                          (push display pieces)
                          (setq pos next-pos)
                          (setq pieces
                                (llm-test--append-events
                                 after-table pos pieces)))
                      (let ((next-pos
                             (llm-test--next-boundary pos end)))
                        (push (buffer-substring-no-properties pos next-pos)
                              pieces)
                        (setq pos next-pos)
                        (setq pieces
                              (llm-test--append-events
                               after-table pos pieces))))))
                (setq pieces
                      (llm-test--append-events before-table pos pieces))
                (apply #'concat (nreverse pieces))))))
       (let* ((wins (window-list nil 'no-minibuf))
              (parts nil))
         (dolist (w wins)
           (let* ((buf (window-buffer w))
                  (name (buffer-name buf))
                  (sel (eq w (selected-window)))
                  (contents (llm-test--window-contents w))
                  (mode (with-current-buffer buf
                          (symbol-name major-mode)))
                  (pt (with-current-buffer buf
                        (point))))
             (push (format
                    \"{\\\"buffer\\\": %s, \\\"mode\\\": %s, \\\"selected\\\": %s, \\\"point\\\": %d, \\\"contents\\\": %s}\"
                    (json-encode-string name)
                    (json-encode-string mode)
                    (if sel \"true\" \"false\")
                    pt
                    (json-encode-string contents))
                   parts)))
         (let ((mini (if (active-minibuffer-window)
                         (with-current-buffer
                             (window-buffer (active-minibuffer-window))
                           (format
                            \"{\\\"active\\\": true, \\\"prompt\\\": %s, \\\"input\\\": %s}\"
                            (json-encode-string (or (minibuffer-prompt) \"\"))
                            (json-encode-string
                             (minibuffer-contents-no-properties))))
                       \"{\\\"active\\\": false}\")))
           (format \"{\\\"windows\\\": [%s], \\\"minibuffer\\\": %s}\"
                   (mapconcat #'identity (nreverse parts) \", \")
                   mini)))))"
  "Elisp expression that captures the full frame state as a JSON string.
Returns a JSON object with `windows' (array of window objects each
having buffer, mode, selected, point, contents) and `minibuffer'
\(object with active, and optionally prompt and input).")

(defun llm-test--capture-frame-state-async (emacs-info)
  "Capture the current frame state of the test Emacs as a JSON string.
EMACS-INFO is the plist from `llm-test--start-emacs'.
Returns a futur that resolves to the JSON string."
  (futur-bind
   (llm-test--eval-in-emacs-async emacs-info llm-test--frame-state-elisp)
   #'futur-done
   (lambda (err)
     (futur-done (format "{\"error\": \"%s\"}" err)))))

(defun llm-test--truncate-result (result)
  "Truncate RESULT string if it exceeds `llm-test-max-tool-result-length'."
  (if (and (stringp result)
           (> (length result) llm-test-max-tool-result-length))
      (concat (substring result 0 llm-test-max-tool-result-length)
              "\n... [truncated]")
    result))

(defun llm-test--wrap-tool-fn (name fn emacs-info)
  "Wrap tool function FN with debug logging, truncation, and frame state.
NAME is the tool name for logging.  EMACS-INFO is the subprocess plist
used to capture frame state after each call.
FN is an async tool function that takes a callback as its first argument.
The returned wrapper is also async (takes callback as first arg)."
  (lambda (callback &rest args)
    (let ((start-time (float-time)))
      (when llm-test-debug
        (message "llm-test tool %s called with: %S" name args))
      (apply fn
             (lambda (raw-result)
               (when llm-test-debug
                 (message "llm-test tool %s returned in %.3fs: %s"
                          name
                          (- (float-time) start-time)
                          (truncate-string-to-width
                           (format "%S" raw-result) 200)))
               (futur-bind
                (llm-test--capture-frame-state-async emacs-info)
                (lambda (frame-state)
                  (let ((result
                         (llm-test--truncate-result
                          (format "%s\n\nFrame state:\n%s"
                                  raw-result frame-state))))
                    (when llm-test-debug
                      (message "llm-test tool %s frame-state captured in %.3fs"
                               name
                               (- (float-time) start-time)))
                    (funcall callback result)
                    (futur-done nil)))))
             args))))

(defun llm-test--make-tools (emacs-info suggestions)
  "Create the list of `llm-tool' structs for the test agent.
EMACS-INFO is the plist from `llm-test--start-emacs'.
SUGGESTIONS is a list (mutated by reference) that accumulates
improvement suggestions from the agent."
  (list
   (make-llm-tool
    :function (lambda (callback code)
                (futur-bind
                 (llm-test--eval-in-emacs-async emacs-info code)
                 (lambda (result)
                   (funcall callback result)
                   (futur-done nil))
                 (lambda (err)
                   (funcall callback
                            (format "ERROR: %s" err))
                   (futur-done nil))))
    :name "eval-elisp"
    :async t
    :description "Evaluate an Emacs Lisp expression in the test Emacs process and return the printed result."
    :args (list (list :name "code" :type 'string
                      :description "The Emacs Lisp expression to evaluate, as a string.")))

   (make-llm-tool
    :function (lambda (callback keys)
                (futur-bind
                 (llm-test--eval-in-emacs-async
                  emacs-info
                  (format
                   "(progn
                      (setq unread-command-events
                            (append unread-command-events
                                    (listify-key-sequence (kbd %S))))
                      (format \"Keys queued (%%s pending)\"
                              (length unread-command-events)))"
                   keys))
                 (lambda (result)
                   (funcall callback result)
                   (futur-done nil))
                 (lambda (err)
                   (funcall callback
                            (format "ERROR: %s" err))
                   (futur-done nil))))
    :name "send-keys"
    :async t
    :description "Send a key sequence to the test Emacs, as if typed by a user.
Use Emacs key notation (e.g. \"C-x C-f\", \"M-x\", \"RET\").

This is non-blocking: it queues the keys and returns immediately.
The keys are processed by the Emacs command loop after this call
returns.  The frame state in the response may not yet reflect the
effect of the keys; call eval-elisp with a no-op expression such as
\"t\" to get an updated frame state if needed.

If a key triggers a command that prompts for input (completing-read,
read-string, etc.), the minibuffer will become active, which will be
visible in the frame state.  Call send-keys again with the response
\(e.g. \"D O N E RET\").  The keys are fed directly into the active
prompt."
    :args (list (list :name "keys" :type 'string
                      :description "Key sequence in Emacs notation.")))

   (make-llm-tool
    :function (lambda (callback suggestion)
                (nconc suggestions (list suggestion))
                (funcall callback
                         (format "Suggestion recorded: %s" suggestion)))
    :name "suggest-improvement"
    :async t
    :description "Record a suggestion for improving the UI or behavior of the
package under test.  This does NOT affect the pass/fail verdict — call it
whenever you notice something that could be better (confusing labels,
missing feedback, awkward key bindings, poor layout, etc.).  You may call
this zero or more times during a test."
    :args (list (list :name "suggestion" :type 'string
                      :description "A description of the improvement you suggest.")))

   (make-llm-tool
    :function (lambda (callback reason)
                (funcall callback (format "PASS: %s" reason)))
    :name "pass-test"
    :async t
    :description "Signal that the current test has PASSED.  Call this when you have verified the expected outcome."
    :args (list (list :name "reason" :type 'string
                      :description "Explanation of why the test passed.")))

   (make-llm-tool
    :function (lambda (callback reason)
                (funcall callback (format "FAIL: %s" reason)))
    :name "fail-test"
    :async t
    :description "Signal that the current test has FAILED.  Call this when the observed behavior does not match expectations."
    :args (list (list :name "reason" :type 'string
                      :description "Explanation of why the test failed.")))))

(defun llm-test--apply-tool-wrapping (tools emacs-info)
  "Wrap each tool in TOOLS with truncation, debug logging, and frame state.
EMACS-INFO is the subprocess plist used to capture frame state."
  (dolist (tool tools)
    (setf (llm-tool-function tool)
          (llm-test--wrap-tool-fn (llm-tool-name tool)
                                  (llm-tool-function tool)
                                  emacs-info)))
  tools)

(defconst llm-test--system-prompt
  "You are an Emacs test agent.  You are given a test description in natural \
language and you must execute it step by step in a fresh Emacs process using \
the provided tools.

Every tool response includes a \"Frame state\" section at the end, which is a \
JSON snapshot of the entire Emacs frame.  The JSON has this structure:

{
  \"windows\": [
    {
      \"buffer\": \"<buffer name>\",
      \"mode\": \"<major mode>\",
      \"selected\": true/false,
      \"point\": <integer>,
      \"contents\": \"<visible text in window>\"
    }
  ],
  \"minibuffer\": {
    \"active\": true/false,
    \"prompt\": \"<prompt text>\",   // only when active
    \"input\": \"<input so far>\"    // only when active
  }
}

The window contents show only what is visible on screen (like a human looking \
at the Emacs frame), not the full buffer.  Use send-keys to scroll (e.g. \
\"C-v\" / \"M-v\") and then call any tool to get an updated frame state.

Your workflow:
1. Read the setup instructions and execute them using eval-elisp or send-keys.
2. Read the test description and perform the actions described.
3. After performing the actions, verify the expected outcome using the frame \
state and/or eval-elisp.
4. Call pass-test if the outcome matches expectations, or fail-test if it \
does not.

Important rules:
- Always call exactly one of pass-test or fail-test before finishing.
- Use eval-elisp for programmatic operations and state inspection.
- Use send-keys when the test requires simulating interactive user input.
- send-keys is non-blocking: the keys may not be processed before the frame \
state is captured.  Call eval-elisp with \"t\" if you need a fresh snapshot.
- Do not batch send-keys across UI state transitions such as opening the \
minibuffer, entering an insertion mode, or waiting for a prompt.  Send a small \
action, refresh, then respond to what is visible.
- When the minibuffer is active (visible in the frame state), respond to the \
prompt appropriately: for read-string/completing-read style prompts, send the \
input and RET; for single-key prompts such as y-or-n-p, send just the single \
key.
- Trust the commands and keybindings named in the test description and setup. \
Do not inspect keymaps, run describe-mode/describe-function, or open help \
buffers unless the test explicitly asks for that or the instructed action has \
already failed more than once.
- eval-elisp runs in a neutral evaluation context, not necessarily the selected \
window's buffer.  Use with-current-buffer when checking buffer-local state.
- If an operation returns an error, try to understand why and report it \
via fail-test.
- Be efficient: prefer performing the requested action and checking the result \
over exploratory introspection.
- If you seem blocked after one or two reasonable recovery attempts, call \
fail-test with the blocker instead of continuing to probe indefinitely.
- Be thorough: verify the actual state, don't assume operations succeeded.
- If you notice anything about the UI or behavior that could be improved \
(confusing text, poor layout, missing feedback, awkward workflow), call \
suggest-improvement to record it.  This is independent of pass/fail."
  "System prompt for the LLM test agent.")

(cl-defstruct llm-test-result
  "The result of running a single test."
  passed-p reason suggestions)

(defun llm-test--run-test-async (provider prompt iteration suggestions callback)
  "Run one async iteration of the agent loop.
PROVIDER is the LLM provider.  PROMPT is the chat prompt with tools.
ITERATION is the current iteration count.  SUGGESTIONS is the shared
list accumulating improvement suggestions.  CALLBACK is called with
an `llm-test-result' when the agent reaches a verdict or hits the
iteration limit."
  (if (> iteration llm-test-max-iterations)
      (funcall callback
               (make-llm-test-result
                :passed-p nil
                :reason (format "Agent did not reach a verdict after %d iterations"
                                llm-test-max-iterations)
                :suggestions (cdr suggestions)))
    (let ((start-time (float-time))
          (llm-warn-on-nonfree nil))
      (when llm-test-debug
        (message "llm-test iteration %d: calling llm-chat-async" iteration))
      (llm-chat-async
       provider prompt
       (lambda (result)
         (when llm-test-debug
           (message "llm-test iteration %d: got response in %.3fs, tool-results=%S"
                    iteration
                    (- (float-time) start-time)
                    (and (plistp result)
                         (plist-get result :tool-results))))
         (let* ((tool-results (plist-get result :tool-results))
                (pass-result (assoc-default "pass-test" tool-results))
                (fail-result (assoc-default "fail-test" tool-results)))
           (cond
            (pass-result
             (funcall callback
                      (make-llm-test-result :passed-p t :reason pass-result
                                            :suggestions (cdr suggestions))))
            (fail-result
             (funcall callback
                      (make-llm-test-result :passed-p nil :reason fail-result
                                            :suggestions (cdr suggestions))))
            (t
             (llm-test--run-test-async provider prompt (1+ iteration)
                                       suggestions callback)))))
       (lambda (_ err)
         (when llm-test-debug
           (message "llm-test iteration %d: LLM error after %.3fs: %s"
                    iteration
                    (- (float-time) start-time)
                    err))
         (funcall callback
                  (make-llm-test-result
                   :passed-p nil
                   :reason (format "LLM error: %s" err)
                   :suggestions (cdr suggestions))))
       t))))

(defun llm-test--run-test (provider emacs-info group-setup test-spec)
  "Run a single test using PROVIDER against EMACS-INFO.
GROUP-SETUP is the setup string for the test group.
TEST-SPEC is an `llm-test-spec' struct.
Returns an `llm-test-result'.  Processes async events while waiting
so that Emacs remains responsive."
  (let* ((user-message
          (format "Setup instructions:\n%s\n\nTest to execute:\n%s"
                  group-setup
                  (llm-test-spec-description test-spec)))
         (suggestions (list nil))
         (tools (llm-test--apply-tool-wrapping
                 (llm-test--make-tools emacs-info suggestions)
                 emacs-info))
         (prompt (let ((llm-warn-on-nonfree nil))
                   (llm-make-chat-prompt
                    user-message
                    :context llm-test--system-prompt
                    :tools tools)))
         (done nil)
         (final-result nil))
    (llm-test--run-test-async
     provider prompt 1 suggestions
     (lambda (result)
       (setq final-result result
             done t)))
    (while (not done)
      ;; Process all events (user input, redisplay, timers, subprocess
      ;; I/O) so that Emacs remains responsive during the test.
      (sit-for 0.1))
    final-result))

;;; Test Loading

(defun llm-test--slugify (string)
  "Convert STRING to a symbol-safe slug."
  (intern
   (replace-regexp-in-string
    "-+" "-"
    (replace-regexp-in-string
     "[^a-z0-9-]" "-"
     (downcase (string-trim string))))))

(defun llm-test-load-directory (directory)
  "Scan DIRECTORY for .yaml/.yml files and parse them all.
Returns a list of `llm-test-group' structs."
  (let ((files (append (directory-files directory t "\\.yaml\\'")
                       (directory-files directory t "\\.yml\\'"))))
    (mapcar #'llm-test--parse-yaml-file files)))

(cl-defun llm-test-register-tests (directory &key provider extra-load-path
                                             init-forms)
  "Register YAML test specs from DIRECTORY with ERT.
PROVIDER is the LLM provider to use; defaults to `llm-test-provider'
or the Elisp in `llm-test-provider-elisp-environment-variable'.

EXTRA-LOAD-PATH is a list of directories to add to the test subprocess
`load-path'.

INIT-FORMS is a list of elisp forms to evaluate in the subprocess at startup."
  (let ((groups (llm-test-load-directory directory)))
    (dolist (group groups)
      (let ((group-slug (llm-test--slugify (llm-test-group-name group)))
            (setup (llm-test-group-setup group)))
        (cl-loop for test in (llm-test-group-tests group)
                 for idx from 1
                 for test-name = (intern (format "llm-test/%s/%d" group-slug idx))
                 for description = (llm-test-spec-description test)
                 do (let ((the-test test)
                          (the-setup setup)
                          (the-provider provider)
                          (the-load-path extra-load-path)
                          (the-init-forms init-forms))
                      (ert-set-test
                       test-name
                       (make-ert-test
                        :name test-name
                        :documentation (format "LLM test: %s (test %d)\n%s"
                                               (llm-test-group-name group)
                                               idx description)
                        :body (lambda ()
                                (let ((emacs-info (llm-test--start-emacs
                                                   :extra-load-path the-load-path
                                                   :init-forms the-init-forms)))
                                  (unwind-protect
                                      (let ((result (llm-test--run-test
                                                     (llm-test--resolve-provider
                                                      the-provider)
                                                     emacs-info
                                                     the-setup the-test)))
                                        (llm-test--report-result result))
                                    (llm-test--stop-emacs emacs-info))))))))))))

(defun llm-test--report-result (result)
  "Report RESULT as an ERT pass or failure.
When `llm-test-warnings-as-errors' is non-nil, any suggestions
cause a failure even if the test passed."
  (let ((suggestions (llm-test-result-suggestions result)))
    (when suggestions
      (message "llm-test suggestions:\n%s"
               (mapconcat (lambda (s) (format "  - %s" s))
                          suggestions "\n")))
    (cond
     ((not (llm-test-result-passed-p result))
      (ert-fail (llm-test-result-reason result)))
     ((and llm-test-warnings-as-errors suggestions)
      (ert-fail
       (format "Test passed but had suggestions (warnings-as-errors):\n%s"
               (mapconcat (lambda (s) (format "  - %s" s))
                          suggestions "\n")))))))

(provide 'llm-test)
;;; llm-test.el ends here
