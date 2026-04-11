;;; llm-test.el --- LLM-driven testing for packages -*- lexical-binding: t -*-

;; Copyright (c) 2026  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/llm-test
;; Package-Requires: ((emacs "29.1") (llm "0.18.0") (yaml "0.5.0") (futur "1.2"))
;; Keywords: testing, tools
;; Version: 0.2.2
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
;;   ;; or set LLM_TEST_DEBUG=file LLM_TEST_PROVIDER_ELISP in the environment
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
(require 'json)

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
  description expected-failure)

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
    - description: <test description>
      expected-failure: <boolean>"
  (let* ((parsed (yaml-parse-string yaml-string))
         (group-name (gethash 'group parsed))
         (setup (gethash 'setup parsed))
         (tests-array (gethash 'tests parsed))
         (tests (mapcar (lambda (test-hash)
                          (make-llm-test-spec
                           :description (gethash 'description test-hash)
                           :expected-failure (gethash 'expected-failure test-hash)))
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

(defconst llm-test--frame-state-helper-elisp
  "(require 'cl-lib)
(require 'json)

(defun llm-test--plain-string (value)
  (cond
   ((stringp value) (substring-no-properties value))
   ((and (consp value) (stringp (car value)))
    (substring-no-properties (car value)))
   (t nil)))

(defun llm-test--display-replacement (value)
  (cond
   ((null value) nil)
   ((llm-test--plain-string value))
   (t \"[display]\")))

(defun llm-test--add-event (table pos string)
  (when (and string pos)
    (puthash pos (append (gethash pos table) (list string)) table)))

(defun llm-test--append-events (table pos pieces)
  (dolist (string (gethash pos table))
    (push string pieces))
  pieces)

(defun llm-test--display-state-at (pos end)
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

(defun llm-test--next-boundary (pos end)
  (min end
       (next-overlay-change pos)
       (next-single-char-property-change pos 'display nil end)))

(defun llm-test--next-display-change (pos end active-state)
  (let ((next (llm-test--next-boundary pos end)))
    (while (and (< next end)
                (equal (llm-test--display-state-at next end)
                       active-state))
      (setq next (llm-test--next-boundary next end)))
    next))

(defun llm-test--window-contents-range (w start-pos end-pos)
  (with-current-buffer (window-buffer w)
    (let* ((overlays (cl-remove-duplicates
                      (append (overlays-in start-pos end-pos)
                              (overlays-at start-pos)
                              (overlays-at end-pos))
                      :test #'eq))
           (before-table (make-hash-table :test #'eql))
           (after-table (make-hash-table :test #'eql))
           (pieces nil)
           (pos start-pos))
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
                     (<= start-pos ov-start) (<= ov-start end-pos))
            (llm-test--add-event before-table ov-start before))
          (when (and display ov-start ov-end (= ov-start ov-end)
                     (<= start-pos ov-start) (<= ov-start end-pos))
            (llm-test--add-event before-table ov-start display))
          (when (and after ov-end
                     (<= start-pos ov-end) (<= ov-end end-pos))
            (llm-test--add-event
             (if (and ov-start (= ov-start ov-end))
                 before-table
               after-table)
             ov-end
             after))))
      (while (< pos end-pos)
        (setq pieces
              (llm-test--append-events before-table pos pieces))
        (let ((display-state
               (llm-test--display-state-at pos end-pos)))
          (if display-state
              (let* ((display (car display-state))
                     (next-pos
                      (llm-test--next-display-change
                       pos end-pos display-state)))
                (push display pieces)
                (setq pos next-pos)
                (setq pieces
                      (llm-test--append-events
                       after-table pos pieces)))
            (let ((next-pos
                   (llm-test--next-boundary pos end-pos)))
              (push (buffer-substring-no-properties pos next-pos)
                    pieces)
              (setq pos next-pos)
              (setq pieces
                    (llm-test--append-events
                     after-table pos pieces))))))
      (setq pieces
            (llm-test--append-events before-table pos pieces))
      (apply #'concat (nreverse pieces)))))

(defun llm-test--window-lines (w)
  (with-current-buffer (window-buffer w)
    (save-excursion
      (save-restriction
        (widen)
        (let* ((start (window-start w))
               (end (window-end w t))
               (lines nil))
          (goto-char start)
          (while (< (point) end)
            (let ((line-start (point)))
              (vertical-motion 1 w)
              (let ((line-end (point)))
                (if (<= line-end line-start)
                    (goto-char end) ; avoid infinite loop
                  (push (llm-test--window-contents-range w line-start line-end)
                        lines)))))
          (nreverse lines))))))

(defun llm-test--capture-frame-state ()
  (redisplay t)
  (let* ((wins (window-list nil 'no-minibuf))
         (windows (make-vector 0 nil))
         (win-num 0)
         (selected-win-num nil))
    (dolist (w wins)
      (let* ((buf (window-buffer w))
             (name (buffer-name buf)))
        (unless (string= name \"*Warnings*\")
          (when (eq w (selected-window))
            (setq selected-win-num win-num))
          (setq windows
                (vconcat windows
                         (list
                          (list :number win-num
                                :buffer name
                                :mode (with-current-buffer buf (symbol-name major-mode))
                                :point (with-current-buffer buf (point))
                                :lines (apply #'vector (llm-test--window-lines w))))))
          (setq win-num (1+ win-num)))))
    (let ((mini-win (active-minibuffer-window)))
      (json-encode
       (list :selected-window
             (list :number selected-win-num
                   :buffer (buffer-name (window-buffer (selected-window))))
             :windows windows
             :minibuffer (if mini-win
                             (with-current-buffer (window-buffer mini-win)
                               (list :active t
                                     :prompt (or (minibuffer-prompt) \"\")
                                     :input (minibuffer-contents-no-properties)))
                           (list :active nil)))))))"
  "Elisp code to be loaded in the test Emacs to support frame state capture.")

(defconst llm-test--frame-state-elisp
  "(llm-test--capture-frame-state)"
  "Elisp expression that captures the full frame state as a JSON string.
Returns a JSON object with `windows' (array of window objects each
having buffer, mode, selected, point, lines) and `minibuffer'
\(object with active, and optionally prompt and input).")
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
              ;; Disable native compilation to avoid noise in tests.
              (insert "(setq native-comp-deferred-compilation nil)\n")
              (insert llm-test--frame-state-helper-elisp "\n")
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

(defun llm-test--wrap-eval-with-tempfile (sexp tempfile)
  "Return elisp that evaluate SEXP and writes the printed result to TEMPFILE.

Used to work around an Emacs server bug: `server-reply-print' chunks
its responses by *character* count even though `server-msg-size' is a
*byte* budget, so long replies that contain multibyte characters
overflow emacsclient's read buffer mid-stream.  emacsclient then
prints `*ERROR*: Unknown message: …' lines into the response, losing
data.  Routing the result through a temp file means the only thing
the protocol has to carry is the success/failure marker, which is
small and pure ASCII.

The wrapped form writes either the `pp'-printed result or, on error,
a single line beginning with `LLM-TEST-ERROR: ' followed by the error
message.  It returns t on success and the symbol `llm-test-error' on
failure so the caller can fall back on the marker if reading the file
fails."
  (format "(condition-case llm-test--err
              (let ((llm-test--result (progn %s)))
                (with-temp-file %S
                  (let ((coding-system-for-write 'utf-8)
                        (standard-output (current-buffer))
                        (print-escape-newlines nil)
                        (print-escape-nonascii nil))
                    (pp llm-test--result)))
                t)
            (error
             (ignore-errors
               (with-temp-file %S
                 (let ((coding-system-for-write 'utf-8))
                   (insert \"LLM-TEST-ERROR: \"
                           (error-message-string llm-test--err)))))
             'llm-test-error))"
          sexp tempfile tempfile))

(defun llm-test--read-eval-tempfile (tempfile)
  "Read TEMPFILE as UTF-8 and return its trimmed contents.
Signals an error if the file is missing, empty, or contains an
`LLM-TEST-ERROR:' marker."
  (unless (file-exists-p tempfile)
    (error "Eval result file %s was not written" tempfile))
  (let ((content (with-temp-buffer
                   (let ((coding-system-for-read 'utf-8))
                     (insert-file-contents tempfile))
                   (string-trim (buffer-string)))))
    (when (string-empty-p content)
      (error "Eval result file %s is empty" tempfile))
    (if (string-prefix-p "LLM-TEST-ERROR: " content)
        (error "%s" (substring content (length "LLM-TEST-ERROR: ")))
      content)))

(defun llm-test--eval-in-emacs (emacs-info sexp)
  "Evaluate SEXP in the test Emacs process described by EMACS-INFO.
SEXP should be a string of elisp to evaluate.
Returns the printed result as a string.  Times out after
`llm-test-timeout' seconds.

Uses `call-process' to avoid processing other async events (like
LLM API responses) while waiting, which would cause re-entrant
callbacks.

Routes the result through a host-side temp file rather than through
emacsclient's reply protocol — see
`llm-test--wrap-eval-with-tempfile' for the rationale."
  (let* ((server-name (plist-get emacs-info :server-name))
         (socket-dir (plist-get emacs-info :socket-dir))
         (tempfile (make-temp-file "llm-test-eval-result-"))
         (wrapped (llm-test--wrap-eval-with-tempfile sexp tempfile)))
    (unwind-protect
        (with-temp-buffer
          (let ((exit-code
                 (call-process "emacsclient" nil t nil
                               (format "--socket-name=%s"
                                       (expand-file-name server-name socket-dir))
                               "--eval" wrapped)))
            (unless (= exit-code 0)
              (error "Emacsclient eval failed (exit %d): %s"
                     exit-code (string-trim (buffer-string))))
            (llm-test--read-eval-tempfile tempfile)))
      (ignore-errors (delete-file tempfile)))))

(defun llm-test--eval-in-emacs-async (emacs-info sexp)
  "Evaluate SEXP in the test Emacs process described by EMACS-INFO.
SEXP should be a string of elisp to evaluate.
Returns a `futur' that resolves to the printed result, or signals
an error if emacsclient fails.  Unlike `llm-test--eval-in-emacs',
this does not block the Emacs event loop.

Uses the same temp-file side channel as `llm-test--eval-in-emacs'."
  (let* ((server-name (plist-get emacs-info :server-name))
         (socket-dir (plist-get emacs-info :socket-dir))
         (tempfile (make-temp-file "llm-test-eval-result-"))
         (wrapped (llm-test--wrap-eval-with-tempfile sexp tempfile))
         (buf (generate-new-buffer " *llm-test-eval*" t))
         (cleanup (lambda ()
                    (ignore-errors (delete-file tempfile))
                    (when (buffer-live-p buf) (kill-buffer buf)))))
    (futur-let*
        ((exit-code <- (futur-process-call
                        "emacsclient" nil buf nil
                        (format "--socket-name=%s"
                                (expand-file-name server-name socket-dir))
                        "--eval" wrapped)))
      (let ((stdout (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (string-trim (buffer-string))))))
        (cond
         ((/= exit-code 0)
          (funcall cleanup)
          (futur-failed
           (list 'error
                 (format "Emacsclient eval failed (exit %d): %s"
                         exit-code (or stdout "")))))
         (t
          (condition-case err
              (let ((result (llm-test--read-eval-tempfile tempfile)))
                (funcall cleanup)
                (futur-done result))
            (error
             (funcall cleanup)
             (futur-failed (list 'error (error-message-string err)))))))))))

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
  "When non-nil, log each tool call and its result.
If set to t, logs to *Messages*.
If set to \\='file, logs to a temporary file and prints its location.
If nil, check `llm-test-debug-environment-variable'."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Messages" t)
                 (const :tag "Temporary file" file))
  :group 'llm-test)

(defcustom llm-test-debug-environment-variable "LLM_TEST_DEBUG"
  "Environment variable to check for debug mode when `llm-test-debug` is nil.
If the variable is set to \"file\", logs to a temporary file.
If set to any other non-empty value, logs to *Messages*."
  :type 'string
  :group 'llm-test)

(defun llm-test--resolve-debug ()
  "Resolve the debug mode from `llm-test-debug` or the environment."
  (or llm-test-debug
      (let ((env-val (getenv llm-test-debug-environment-variable)))
        (cond
         ((null env-val) nil)
         ((string= env-val "file") 'file)
         ((not (string= env-val "")) t)
         (t nil)))))


(defvar llm-test--current-debug-file nil
  "The file where debug logs are currently being written.")

(defvar llm-test--start-time nil
  "The `float-time' when the current test started, for elapsed timestamps.")

(defun llm-test--log (format-string &rest args)
  "Log FORMAT-STRING with ARGS, based on `llm-test-debug' or environment.
Each line is prefixed with the elapsed time since `llm-test--start-time'."
  (let* ((debug (llm-test--resolve-debug))
         (elapsed (if llm-test--start-time
                      (- (float-time) llm-test--start-time)
                    0.0))
         (raw (apply #'format format-string args))
         (prefix (format "[+%.1fs] " elapsed))
         (msg (concat prefix raw)))
    (when debug
      (message "llm-test: %s" msg))
    (when llm-test--current-debug-file
      (with-temp-buffer
        (insert msg)
        (newline)
        (append-to-file (point-min) (point-max) llm-test--current-debug-file)))))

(defcustom llm-test-max-tool-result-length 2000
  "Maximum character length for a tool result returned to the LLM.
Results longer than this are truncated with a notice.  This prevents
large return values (e.g. struct representations) from bloating the
conversation context and causing API timeouts."
  :type 'integer
  :group 'llm-test)


(defun llm-test--capture-frame-state-async (emacs-info)
  "Capture the current frame state of the test Emacs as a JSON string.
EMACS-INFO is the plist from `llm-test--start-emacs'.
Returns a futur that resolves to the JSON string."
  (futur-bind
   (llm-test--eval-in-emacs-async emacs-info llm-test--frame-state-elisp)
   (lambda (result)
     (futur-done (condition-case nil (read result) (error result))))
   (lambda (err)
     (futur-done (format "{\"error\": \"%s\"}" err)))))

(defun llm-test--truncate-result (result)
  "Truncate RESULT string if it exceeds `llm-test-max-tool-result-length'."
  (if (and (stringp result)
           (> (length result) llm-test-max-tool-result-length))
      (concat (substring result 0 llm-test-max-tool-result-length)
              "\n... [truncated]")
    result))

(defun llm-test--pretty-json (value)
  "Pretty-print VALUE as JSON.
VALUE may be a JSON string or an already-parsed Elisp object."
  (condition-case nil
      (let ((json-encoding-pretty-print t)
            (parsed (if (stringp value)
                        (json-read-from-string value)
                      value)))
        (json-encode parsed))
    (error (format "%S" value))))

(defun llm-test--wrap-tool-fn (name fn emacs-info)
  "Wrap tool function FN with debug logging, truncation, and frame state.
NAME is the tool name for logging.  EMACS-INFO is the subprocess plist
used to capture frame state after each call.
FN is an async tool function that takes a callback as its first argument.
The returned wrapper is also async (takes callback as first arg)."
  (lambda (callback &rest args)
    (let ((start-time (float-time)))
      (llm-test--log "[Tool Call: %s]\n  Args: %S" name args)
      (apply fn
             (lambda (raw-result)
               (llm-test--log "[Tool Result: %s (%.1fs)]\n  Result: %s"
                              name
                              (- (float-time) start-time)
                              (truncate-string-to-width
                               (format "%s" raw-result) 500))
               (futur-bind
                (llm-test--capture-frame-state-async emacs-info)
                (lambda (frame-state)
                  (let* ((pretty-state (llm-test--pretty-json frame-state))
                         (result
                          (llm-test--truncate-result
                           (format "%s\n\nFrame state:\n%s"
                                   raw-result frame-state))))
                    (llm-test--log "[Frame State after %s]\n%s"
                                   name pretty-state)
                    (funcall callback result)
                    (futur-done nil)))
                (lambda (err)
                  (llm-test--log "[Frame State FAILED for %s]: %S"
                                 name err)
                  (funcall callback raw-result)
                  (futur-done nil))))
             args))))

(defconst llm-test--reserved-key-names
  '("RET" "SPC" "TAB" "ESC" "DEL" "BS" "NUL" "LFD"
    "BACKSPACE" "RETURN" "ESCAPE" "INSERT"
    "HOME" "END" "UP" "DOWN" "LEFT" "RIGHT" "PRIOR" "NEXT")
  "Token names that the kbd parser treats as named keys, not literal text.")

(defun llm-test--key-token-text-p (token)
  "Return non-nil if TOKEN should be treated as literal text by `send-keys'.
Returns nil if TOKEN looks like a kbd-notation key (modifier prefix,
angle-bracketed function key, reserved key name, or single character)."
  (and (> (length token) 1)
       (not (string-match-p "\\`[CSAHMs]-" token))
       (not (string-match-p "\\`<.*>\\'" token))
       (not (member token llm-test--reserved-key-names))))

(defun llm-test--rewrite-key-sequence (keys)
  "Rewrite KEYS so spaces between literal-text tokens become real spaces.

The kbd parser treats every space as a token separator and discards
literal spaces, so a phrase like \"do taxes RET\" parses as the keys
\"d o t a x e s RET\" — with the space between \"do\" and \"taxes\"
silently dropped.  This helper inserts an explicit \"SPC\" token between
each pair of adjacent literal-text tokens so the parsed key sequence
preserves the intended spacing.  Tokens that look like kbd notation
\(modifier chords, function keys, reserved key names, single
characters) are left untouched, so existing call sites such as
`M-x ekg-org-view RET' continue to work."
  (let* ((tokens (split-string keys "[ \t\n]+" t))
         (result nil)
         (prev-text nil))
    (dolist (token tokens)
      (let ((is-text (llm-test--key-token-text-p token)))
        (when (and is-text prev-text)
          (push "SPC" result))
        (push token result)
        (setq prev-text is-text)))
    (mapconcat #'identity (nreverse result) " ")))

(defun llm-test--make-tools (emacs-info suggestions)
  "Create the list of `llm-tool' structs for the test agent.
EMACS-INFO is the plist from `llm-test--start-emacs'.
SUGGESTIONS is a list (mutated by reference) that accumulates
improvement suggestions from the agent."
  (list
   (make-llm-tool
    :function (lambda (callback code)
                (let ((wrapped (format "(with-current-buffer (window-buffer (selected-window)) %s)" code)))
                  (futur-bind
                   (llm-test--eval-in-emacs-async emacs-info wrapped)
                   (lambda (result)
                     (funcall callback result)
                     (futur-done nil))
                   (lambda (err)
                     (funcall callback
                              (format "ERROR: %s" err))
                     (futur-done nil)))))
    :name "eval-elisp"
    :async t
    :description "Evaluate an Emacs Lisp expression in the test Emacs process and return the printed result.
The expression is evaluated in the context of the currently selected window's buffer."
    :args (list (list :name "code" :type 'string
                      :description "The Emacs Lisp expression to evaluate, as a string.")))

   (make-llm-tool
    :function (lambda (callback text)
                (futur-bind
                 (llm-test--eval-in-emacs-async
                  emacs-info
                  (format "(with-current-buffer (window-buffer (selected-window)) (execute-kbd-macro %S) \"Text inserted\")" text))
                 (lambda (result)
                   (funcall callback result)
                   (futur-done nil))
                 (lambda (err)
                   (funcall callback (format "ERROR: %s" err))
                   (futur-done nil))))
    :name "type-text"
    :async t
    :description "Type literal text into the current buffer at point. Use this for entering paragraphs, sentences, or any strings where spaces and characters should be preserved exactly as-is."
    :args (list (list :name "text" :type 'string
                      :description "The literal text to type.")))

   (make-llm-tool
    :function (lambda (callback command)
                (futur-bind
                 (llm-test--eval-in-emacs-async
                  emacs-info
                  (format
                   "(with-current-buffer (window-buffer (selected-window))
                      (call-interactively '%s)
                      \"Command executed\")"
                   command))
                 (lambda (result)
                   (funcall callback result)
                   (futur-done nil))
                 (lambda (err)
                   (funcall callback (format "ERROR: %s" err))
                   (futur-done nil))))
    :name "run-command"
    :async t
    :description "Run an Emacs interactive command by name in the currently
selected window's buffer.  This is equivalent to the user typing
`M-x COMMAND RET' but without going through the minibuffer.

Use this when you know the exact command name — it is more reliable
than typing it through `send-keys' with `M-x', since it avoids
minibuffer completion issues and typos.

If the command reads input from the minibuffer (e.g. a file name or
search string), the minibuffer will become active after this call
and you should answer it with `type-text' or `send-keys'."
    :args (list (list :name "command" :type 'string
                      :description "The Emacs command name (e.g. \"ekg-org-view\", \"save-buffer\").")))

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
                   (llm-test--rewrite-key-sequence keys)))
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

The input is parsed as space-separated tokens.  Tokens that look like
kbd notation — modifier chords (\"C-c\", \"M-x\"), function keys
(\"<f1>\", \"<up>\"), reserved names (\"RET\", \"SPC\", \"TAB\",
\"ESC\", \"DEL\", \"BACKSPACE\", arrow names, etc.), and single
characters — are dispatched as keys.  Multi-character word tokens are
typed literally, and a literal space character is inserted between
each pair of adjacent word tokens so phrases survive intact.

Examples:
- \"M-x ekg-org-view RET\"   -> M-x, type \"ekg-org-view\", RET
- \"do taxes RET\"           -> type \"do taxes\", RET
- \"C-x C-f /tmp/foo RET\"   -> C-x C-f, type \"/tmp/foo\", RET
- \"H e l l o SPC w o r l d\"-> single-char keys with explicit SPC

For typing long paragraphs of literal text, prefer the `type-text'
tool.

This is non-blocking: it queues the keys and returns immediately.
The keys are processed by the Emacs command loop after this call
returns.  The frame state in the response may not yet reflect the
effect of the keys; call eval-elisp with a no-op expression such as
\"t\" to get an updated frame state if needed.

If a key triggers a command that prompts for input (completing-read,
read-string, etc.), the minibuffer will become active, which will be
visible in the frame state.  Call send-keys again with the response
\(e.g. \"DONE RET\").  The keys are fed directly into the active
prompt."
    :args (list (list :name "keys" :type 'string
                      :description "Key sequence in Emacs notation.")))

   (make-llm-tool
    :function (lambda (callback seconds)
                (let ((secs (max 0.1 (min (float seconds) 60))))
                  (run-at-time secs nil
                               (lambda ()
                                 (funcall callback
                                          (format "Slept %.1f seconds" secs))))))
    :name "sleep"
    :async t
    :description "Pause for the given number of seconds before returning.
Use this when waiting for an asynchronous operation (e.g. an agent
spawned by a command) to make progress.  The frame state in the
response is captured AFTER the sleep, so it will reflect any changes
that occurred during the wait.  Maximum 60 seconds per call."
    :args (list (list :name "seconds" :type 'number
                      :description "Number of seconds to sleep (max 60).")))

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
                      :description "Explanation of why the test failed.")))

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
  \"selected-window\": {\"number\": <int>, \"buffer\": \"<buffer name>\"},
  \"windows\": [
    {
      \"number\": <int>,
      \"buffer\": \"<buffer name>\",
      \"mode\": \"<major mode>\",
      \"point\": <integer>,
      \"lines\": [\"<visual line 1>\", \"<visual line 2>\", ...]
    }
  ],
  \"minibuffer\": {
    \"active\": true/false,
    \"prompt\": \"<prompt text>\",   // only when active
    \"input\": \"<input so far>\"    // only when active
  }
}

The top-level \"selected-window\" tells you which window currently has focus \
(by number and buffer name).  Any keys or commands you send will act on that \
window.  If it is not the window you need, switch to the right one before \
continuing (e.g. run-command \"switch-to-buffer\" or send-keys \"C-x b\").  \
Note that the same buffer can appear in multiple windows.

The window \"lines\" field is an array of strings, each representing a single \
visual line on the screen.  If a long line of text is wrapped by Emacs \
(e.g. via visual-line-mode or auto-fill-mode), it will appear as multiple \
entries in the \"lines\" array.  This represents exactly what a human \
looking at the Emacs frame would see.

Important rules:
- Always call exactly one of pass-test or fail-test before finishing.
- Use eval-elisp for programmatic operations and state inspection.
- Use run-command to call an Emacs command by name (e.g. run-command \
\"ekg-org-view\").  This is more reliable than typing \"M-x command-name RET\" \
via send-keys and should be preferred when you know the command name.
- Use type-text for typing strings of text (sentences, paragraphs) into the \
buffer or minibuffer.  This preserves spaces and special characters.
- Use send-keys for key sequences such as RET, TAB, C-c C-c, or single-key \
commands.  Spaces between multi-character word tokens are preserved \
automatically, but prefer type-text for longer text input.
- send-keys is non-blocking: the keys may not be processed before the frame \
state is captured.  Call eval-elisp with \"t\" if you need a fresh snapshot.
- Do not batch send-keys across UI state transitions such as opening the \
minibuffer, entering an insertion mode, or waiting for a prompt.  Send a small \
action, refresh, then respond to what is visible.
- When the minibuffer is active (visible in the frame state), respond to the \
prompt appropriately: for read-string/completing-read style prompts, send the \
input and RET; for single-key prompts such as y-or-n-p, send just the single \
key.
- Emacs navigation basics (use these via send-keys or run-command as needed):
  * Switch to a buffer: run-command \"switch-to-buffer\" then type-text the \
buffer name and send RET.  Or use send-keys with \"C-x b\" then the name \
and RET.
  * Switch windows: send-keys \"C-x o\" cycles between visible windows.
  * Go to beginning/end of buffer: send-keys \"M-<\" or \"M->\".
  * Move by line: send-keys \"C-n\" (next) or \"C-p\" (previous).
  * Always check \"selected-window\" at the top of the frame state to know \
which window has focus.  If a command opened a popup or result buffer that \
stole focus, switch back to the buffer you need before continuing.
- Trust the commands and keybindings named in the test description and setup. \
Do not inspect keymaps, run describe-mode/describe-function, or open help \
buffers unless the test explicitly asks for that or the instructed action has \
already failed more than once.
- The frame state is the authoritative, exact view of the buffer contents.  The \
\"lines\" array contains the precise, complete text in the buffer with no \
hidden characters, no missing whitespace, and no truncation — it is exactly \
equivalent to what buffer-string would return.  Use it to verify buffer \
contents, text, wrapping, and layout.  Do NOT call eval-elisp to re-read or \
double-check buffer text that is already visible in the frame state.  Only use \
eval-elisp to check things that are not represented in the frame state (e.g. \
text properties, variable values, or buffer-local state).
- If an operation returns an error, try to understand why and report it \
via fail-test.
- Never try to mock or simulating anything that isn't working.  If you get a \
failure, let it fail without trying to work around it.
- Be efficient: prefer performing the requested action and checking the result \
over exploratory introspection.  After performing an action, check the frame \
state that came back — if it already confirms the expected outcome, you MUST \
call pass-test or fail-test immediately in the same turn without making any \
additional tool calls.  The concatenation of all strings in the \"lines\" array \
is equivalent to calling buffer-string — never call buffer-string, \
buffer-substring, or similar functions to re-read text that is already in the \
frame state.
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
      (llm-test--log "\n========== Iteration %d ==========" iteration)
      (when (= iteration 1)
        (let* ((interactions (llm-chat-prompt-interactions prompt))
               (user-msg (llm-chat-prompt-interaction-content (car interactions))))
          (llm-test--log "[User Message]\n%s" user-msg)))
      (llm-test--log "[Calling LLM...]")
      (llm-chat-async
       provider prompt
       (lambda (result)
         (let ((reasoning (if (plistp result) (plist-get result :reasoning) nil))
               (text (if (plistp result) (plist-get result :text) result))
               (tool-results (if (plistp result) (plist-get result :tool-results) nil)))
           (llm-test--log "[LLM Response (%.1fs)]"
                          (- (float-time) start-time))
           (when reasoning
             (llm-test--log "[Agent Reasoning]\n%s" reasoning))
           (when (and text (not (equal text "")))
             (llm-test--log "[Agent Text]\n%s" text))
           (when tool-results
             (llm-test--log "[Tool Results Summary]\n  %s"
                            (mapconcat (lambda (tr) (format "%s" (car tr)))
                                       tool-results ", ")))
           (let* (;; TODO: The tool identifier may come back as strings or symbols.  The
                  ;; underlying cause of this needs to be fixed.
                  (pass-result (assoc-default "pass-test" tool-results))
                  (fail-result (assoc-default "fail-test" tool-results)))
             (cond
              (pass-result
               (llm-test--log "\n========== PASS ==========\n  %s" pass-result)
               (funcall callback
                        (make-llm-test-result :passed-p t :reason pass-result
                                              :suggestions (cdr suggestions))))
              (fail-result
               (llm-test--log "\n========== FAIL ==========\n  %s" fail-result)
               (funcall callback
                        (make-llm-test-result :passed-p nil :reason fail-result
                                              :suggestions (cdr suggestions))))
              (t
               (llm-test--run-test-async provider prompt (1+ iteration)
                                         suggestions callback))))))
       (lambda (_ err)
         (llm-test--log "[LLM Error (%.1fs)]: %s"
                        (- (float-time) start-time)
                        err)
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
  (let* ((debug-file (when (eq (llm-test--resolve-debug) 'file)
                       (make-temp-file (expand-file-name "llm-test-debug-" default-directory) nil ".log")))
         (user-message (format "Setup instructions:\n%s\n\nTest to execute:\n%s"
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
    ;; Set global variables so async callbacks (process filters) can see them.
    (setq llm-test--current-debug-file debug-file
          llm-test--start-time (float-time))
    (when debug-file
      (message "llm-test debug log: %s" (expand-file-name debug-file)))
    (llm-test--log "======================================")
    (llm-test--log "Test: %s" (llm-test-spec-description test-spec))
    (llm-test--log "======================================")
    (llm-test--run-test-async
     provider prompt 1 suggestions
     (lambda (result)
       (setq final-result result
             done t)))
    (while (not done)
      ;; Process all events (user input, redisplay, timers, subprocess
      ;; I/O) so that Emacs remains responsive during the test.
      (sit-for 0.1))
    (setq llm-test--current-debug-file nil
          llm-test--start-time nil)
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
                        :expected-result-type (if (llm-test-spec-expected-failure the-test) :failed :passed)
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
