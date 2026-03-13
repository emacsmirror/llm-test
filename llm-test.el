;;; llm-test.el --- LLM-driven testing for Emacs packages -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/llm-test
;; Package-Requires: ((emacs "28.1") (llm "0.18.0") (yaml "0.5.0"))
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
;;   (llm-test-register-tests "path/to/testscripts/")

;;; Code:

(require 'llm)
(require 'yaml)
(require 'ert)
(require 'cl-lib)

(defgroup llm-test nil
  "LLM-driven testing for Emacs packages."
  :group 'tools)

(defcustom llm-test-emacs-executable "emacs"
  "Path to the Emacs executable used to run tests.
A fresh Emacs process (emacs -Q) is launched for each test."
  :type 'string
  :group 'llm-test)

(defcustom llm-test-provider nil
  "The LLM provider to use for the test agent.
This should be an object created by one of the `make-llm-*' constructors
from the `llm' package."
  :type 'sexp
  :group 'llm-test)

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

(cl-defun llm-test--start-emacs (&key load-path init-forms)
  "Start a fresh Emacs process for testing.
LOAD-PATH is a list of directories to add to the subprocess `load-path'.
INIT-FORMS is a list of elisp forms to evaluate at startup.
Returns a plist with :process, :server-name, :socket-dir, and :init-file."
  (let* ((server-name (format "llm-test-%d-%d"
                              (emacs-pid)
                              (cl-incf llm-test--server-name-counter)))
         (socket-dir (make-temp-file "llm-test-socket-" t))
         (init-file (make-temp-file "llm-test-init-" nil ".el"))
         (_ (with-temp-file init-file
              (insert (format "(setq server-socket-dir %S server-name %S)\n"
                              socket-dir server-name))
              (dolist (dir load-path)
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
        (sleep-for 0.1))
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
          (error "emacsclient eval failed (exit %d): %s"
                 exit-code (buffer-string)))))))

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

(defcustom llm-test-max-iterations 20
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

(defun llm-test--truncate-result (result)
  "Truncate RESULT string if it exceeds `llm-test-max-tool-result-length'."
  (if (and (stringp result)
           (> (length result) llm-test-max-tool-result-length))
      (concat (substring result 0 llm-test-max-tool-result-length)
              "\n... [truncated]")
    result))

(defun llm-test--wrap-tool-fn (name fn)
  "Wrap tool function FN with debug logging and result truncation."
  (lambda (&rest args)
    (when llm-test-debug
      (message "llm-test tool %s called with: %S" name args))
    (let ((result (llm-test--truncate-result (apply fn args))))
      (when llm-test-debug
        (message "llm-test tool %s returned: %s" name
                 (truncate-string-to-width (format "%S" result) 200)))
      result)))

(defun llm-test--make-tools (emacs-info suggestions)
  "Create the list of `llm-tool' structs for the test agent.
EMACS-INFO is the plist from `llm-test--start-emacs'.
SUGGESTIONS is a list (mutated by reference) that accumulates
improvement suggestions from the agent."
  (list
   (make-llm-tool
    :function (lambda (code)
                (condition-case err
                    (llm-test--eval-in-emacs emacs-info code)
                  (error (format "ERROR: %s" (error-message-string err)))))
    :name "eval-elisp"
    :description "Evaluate an Emacs Lisp expression in the test Emacs process and return the printed result."
    :args (list (list :name "code" :type 'string
                      :description "The Emacs Lisp expression to evaluate, as a string.")))

   (make-llm-tool
    :function (lambda (buffer-name)
                (condition-case err
                    (llm-test--eval-in-emacs
                     emacs-info
                     (format "(progn
                                (let ((w (get-buffer-window %S t)))
                                  (unless w
                                    (set-window-buffer (selected-window) %S)
                                    (setq w (selected-window)))
                                  (redisplay t)
                                  (with-current-buffer %S
                                    (buffer-substring-no-properties
                                     (window-start w)
                                     (window-end w t)))))"
                             buffer-name buffer-name buffer-name))
                  (error (format "ERROR: %s" (error-message-string err)))))
    :name "get-buffer-contents"
    :description "Get the text currently visible in the window displaying a buffer.
This returns only what would be on screen, not the full buffer.  Use
scroll-down-command / scroll-up-command via send-keys to see more content."
    :args (list (list :name "buffer_name" :type 'string
                      :description "The name of the buffer to read.")))

   (make-llm-tool
    :function (lambda ()
                (condition-case err
                    (llm-test--eval-in-emacs
                     emacs-info
                     "(mapcar #'buffer-name (buffer-list))")
                  (error (format "ERROR: %s" (error-message-string err)))))
    :name "get-buffer-list"
    :description "List all buffer names in the test Emacs."
    :args nil)

   (make-llm-tool
    :function (lambda (keys)
                (condition-case err
                    (llm-test--eval-in-emacs
                     emacs-info
                     (format
                      "(progn
                         (setq unread-command-events
                               (append unread-command-events
                                       (listify-key-sequence (kbd %S))))
                         (format \"Keys queued (%%s pending)\"
                                 (length unread-command-events)))"
                      keys))
                  (error (format "ERROR: %s" (error-message-string err)))))
    :name "send-keys"
    :description "Send a key sequence to the test Emacs, as if typed by a user.
Use Emacs key notation (e.g. \"C-x C-f\", \"M-x\", \"RET\").

This is non-blocking: it queues the keys and returns immediately.
The keys are processed by the Emacs command loop after this call
returns.  After calling send-keys, use get-buffer-contents or
get-minibuffer-contents to observe the effect.

If a key triggers a command that prompts for input (completing-read,
read-string, etc.), the minibuffer becomes active.  Use
get-minibuffer-contents to see the prompt, then call send-keys again
with the response (e.g. \"D O N E RET\").  The keys are fed directly
into the active prompt."
    :args (list (list :name "keys" :type 'string
                      :description "Key sequence in Emacs notation.")))

   (make-llm-tool
    :function (lambda ()
                (condition-case err
                    (llm-test--eval-in-emacs
                     emacs-info
                     "(if (active-minibuffer-window)
                          (with-current-buffer (window-buffer (active-minibuffer-window))
                            (format \"Prompt: %s\\nInput so far: %s\"
                                    (or (minibuffer-prompt) \"\")
                                    (minibuffer-contents-no-properties)))
                        \"No active minibuffer\")")
                  (error (format "ERROR: %s" (error-message-string err)))))
    :name "get-minibuffer-contents"
    :description "Check whether a minibuffer prompt is active and return its contents.
Returns the prompt text and any input typed so far, or a message
indicating no minibuffer is active.  Use this after send-keys to see
if a command opened a prompt that needs a response."
    :args nil)

   (make-llm-tool
    :function (lambda (suggestion)
                (nconc suggestions (list suggestion))
                (format "Suggestion recorded: %s" suggestion))
    :name "suggest-improvement"
    :description "Record a suggestion for improving the UI or behavior of the
package under test.  This does NOT affect the pass/fail verdict — call it
whenever you notice something that could be better (confusing labels,
missing feedback, awkward key bindings, poor layout, etc.).  You may call
this zero or more times during a test."
    :args (list (list :name "suggestion" :type 'string
                      :description "A description of the improvement you suggest.")))

   (make-llm-tool
    :function (lambda (reason) (format "PASS: %s" reason))
    :name "pass-test"
    :description "Signal that the current test has PASSED.  Call this when you have verified the expected outcome."
    :args (list (list :name "reason" :type 'string
                      :description "Explanation of why the test passed.")))

   (make-llm-tool
    :function (lambda (reason) (format "FAIL: %s" reason))
    :name "fail-test"
    :description "Signal that the current test has FAILED.  Call this when the observed behavior does not match expectations."
    :args (list (list :name "reason" :type 'string
                      :description "Explanation of why the test failed.")))))

(defun llm-test--apply-tool-wrapping (tools)
  "Wrap each tool in TOOLS with result truncation and optional debug logging."
  (dolist (tool tools)
    (setf (llm-tool-function tool)
          (llm-test--wrap-tool-fn (llm-tool-name tool)
                                  (llm-tool-function tool))))
  tools)

(defconst llm-test--system-prompt
  "You are an Emacs test agent.  You are given a test description in natural \
language and you must execute it step by step in a fresh Emacs process using \
the provided tools.

Your workflow:
1. Read the setup instructions and execute them using eval-elisp or send-keys.
2. Read the test description and perform the actions described.
3. After performing the actions, verify the expected outcome by inspecting \
the visible buffer contents, evaluating elisp expressions, etc.
4. Call pass-test if the outcome matches expectations, or fail-test if it \
does not.

Important rules:
- Always call exactly one of pass-test or fail-test before finishing.
- Use eval-elisp for programmatic operations and state inspection.
- Use send-keys when the test requires simulating interactive user input.
- send-keys is non-blocking: it schedules the keys and returns immediately.  \
After calling send-keys, call get-buffer-contents or get-minibuffer-contents \
to observe the effect.
- get-buffer-contents returns only what is visible in the window (like a \
human looking at the screen).  To see more content, use send-keys to scroll \
(e.g. \"C-v\" to scroll down, \"M-v\" to scroll up) and then call \
get-buffer-contents again.
- When a key triggers a command that prompts for input (completing-read, \
read-string, etc.), the minibuffer becomes active.  Call \
get-minibuffer-contents to see the prompt, then call send-keys with the \
response (e.g. \"D O N E RET\").  The keys are fed directly into the active \
prompt.
- If an operation returns an error, try to understand why and report it \
via fail-test.
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
    (when llm-test-debug
      (message "llm-test iteration %d: calling llm-chat-async" iteration))
    (llm-chat-async
     provider prompt
     (lambda (result)
       (when llm-test-debug
         (message "llm-test iteration %d: got response, tool-results=%S"
                  iteration (and (plistp result)
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
         (message "llm-test iteration %d: LLM error: %s" iteration err))
       (funcall callback
                (make-llm-test-result
                 :passed-p nil
                 :reason (format "LLM error: %s" err)
                 :suggestions (cdr suggestions))))
     t)))

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
                 (llm-test--make-tools emacs-info suggestions)))
         (prompt (llm-make-chat-prompt
                  user-message
                  :context llm-test--system-prompt
                  :tools tools))
         (done nil)
         (final-result nil))
    (llm-test--run-test-async
     provider prompt 1 suggestions
     (lambda (result)
       (setq final-result result
             done t)))
    (while (not done)
      (accept-process-output nil 0.1))
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

(cl-defun llm-test-register-tests (directory &key provider load-path init-forms)
  "Load YAML test specs from DIRECTORY and register them as ERT tests.
PROVIDER is the LLM provider to use; defaults to `llm-test-provider'.
LOAD-PATH is a list of directories to add to the test subprocess `load-path'.
INIT-FORMS is a list of elisp forms to evaluate in the subprocess at startup."
  (let ((groups (llm-test-load-directory directory))
        (provider (or provider llm-test-provider)))
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
                          (the-load-path load-path)
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
                                                   :load-path the-load-path
                                                   :init-forms the-init-forms)))
                                  (unwind-protect
                                      (let ((result (llm-test--run-test
                                                     the-provider emacs-info
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
