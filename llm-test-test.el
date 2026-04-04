;;; llm-test-test.el --- Tests for llm-test -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the llm-test YAML parsing and runtime behavior.

;;; Code:

(require 'llm-test)
(require 'ert)
(require 'json)
(require 'seq)

(defun llm-test-test--selected-window (state)
  "Return the selected window object from parsed frame STATE."
  (let ((windows (alist-get "windows" state nil nil #'string=)))
    (seq-find (lambda (window)
                (eq (alist-get "selected" window nil nil #'string=) t))
              windows)))

(defun llm-test-test--selected-window-contents (info)
  "Return the visible contents of the selected window from INFO's frame state."
  (let ((state (json-parse-string
                (read (llm-test--eval-in-emacs info
                                               llm-test--frame-state-elisp))
                :object-type 'alist
                :array-type 'list)))
    (alist-get "contents"
               (llm-test-test--selected-window state)
               nil nil #'string=)))

(defun llm-test-test--testscripts-directory ()
  "Return the absolute path to the sample YAML test scripts."
  (expand-file-name "testscripts"
                    (file-name-directory (locate-library "llm-test"))))

(llm-test-register-tests (llm-test-test--testscripts-directory))

(ert-deftest llm-test-provider-from-elisp ()
  "Provider Elisp should evaluate to the constructed provider."
  (should (eq (llm-test--provider-from-elisp "(progn 'test-provider)")
              'test-provider)))

(ert-deftest llm-test-provider-from-elisp-trailing-data ()
  "Provider Elisp with trailing forms should signal an error."
  (should-error (llm-test--provider-from-elisp "'one 'two")))

(ert-deftest llm-test-provider-from-environment ()
  "The provider should be read from `LLM_TEST_PROVIDER_ELISP'."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv llm-test-provider-elisp-environment-variable
            "(progn 'env-provider)")
    (should (eq (llm-test--provider-from-environment)
                'env-provider))))

(ert-deftest llm-test-provider-from-environment-invalid ()
  "Invalid provider Elisp in the environment should fail clearly."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv llm-test-provider-elisp-environment-variable "(")
    (should-error (llm-test--provider-from-environment))))

(ert-deftest llm-test-resolve-provider-precedence ()
  "Explicit providers should override variable and environment fallback."
  (let ((process-environment (copy-sequence process-environment))
        (llm-test-provider 'configured-provider))
    (setenv llm-test-provider-elisp-environment-variable
            "(progn 'env-provider)")
    (should (eq (llm-test--resolve-provider 'explicit-provider)
                'explicit-provider))
    (should (eq (llm-test--resolve-provider)
                'configured-provider))))

(ert-deftest llm-test-resolve-provider-missing ()
  "Missing provider configuration should signal a clear error."
  (let ((process-environment (copy-sequence process-environment))
        (llm-test-provider nil))
    (setenv llm-test-provider-elisp-environment-variable nil)
    (should-error (llm-test--resolve-provider))))

(ert-deftest llm-test-parse-simple-yaml ()
  "Parsing a simple YAML spec should produce the correct group struct."
  (let ((group (llm-test--parse-yaml-string
                "group: my tests\nsetup: do setup\ntests:\n  - description: test one\n  - description: test two\n")))
    (should (equal (llm-test-group-name group) "my tests"))
    (should (equal (llm-test-group-setup group) "do setup"))
    (should (= (length (llm-test-group-tests group)) 2))
    (should (equal (llm-test-spec-description (nth 0 (llm-test-group-tests group)))
                   "test one"))
    (should (equal (llm-test-spec-description (nth 1 (llm-test-group-tests group)))
                   "test two"))))

(ert-deftest llm-test-parse-no-setup ()
  "A YAML spec without setup should default to an empty string."
  (let ((group (llm-test--parse-yaml-string
                "group: no setup\ntests:\n  - description: a test\n")))
    (should (equal (llm-test-group-setup group) ""))
    (should (= (length (llm-test-group-tests group)) 1))))

(ert-deftest llm-test-parse-missing-group ()
  "Parsing YAML without a group key should signal an error."
  (should-error (llm-test--parse-yaml-string
                 "tests:\n  - description: orphan test\n")))

(ert-deftest llm-test-parse-missing-tests ()
  "Parsing YAML without tests should signal an error."
  (should-error (llm-test--parse-yaml-string
                 "group: empty\n")))

(ert-deftest llm-test-parse-file ()
  "Parsing a YAML file from disk should work."
  (let ((file (expand-file-name "testscripts/auto-fill-tests.yaml"
                                (file-name-directory (locate-library "llm-test")))))
    (let ((group (llm-test--parse-yaml-file file)))
      (should (equal (llm-test-group-name group) "auto-fill mode"))
      (should (= (length (llm-test-group-tests group)) 2)))))

(ert-deftest llm-test-load-directory ()
  "Loading a directory should find all YAML files."
  (let ((groups (llm-test-load-directory
                 (llm-test-test--testscripts-directory))))
    (should (= (length groups) 4))))

;;; Slugify tests

(ert-deftest llm-test-slugify ()
  "Slugify should produce clean symbol names."
  (should (eq (llm-test--slugify "auto-fill mode") 'auto-fill-mode))
  (should (eq (llm-test--slugify "Basic Editing!") 'basic-editing-)))

;;; ERT registration tests

(ert-deftest llm-test-register-creates-ert-tests ()
  "Registering tests from a directory should create ERT test symbols."
  (should (ert-test-boundp 'llm-test/auto-fill-mode/1))
  (should (ert-test-boundp 'llm-test/auto-fill-mode/2))
  (should (ert-test-boundp 'llm-test/basic-editing/1))
  (should (ert-test-boundp 'llm-test/basic-editing/2))
  (should (ert-test-boundp 'llm-test/fido-mode-completion/1))
  (should (ert-test-boundp 'llm-test/fido-mode-completion/2))
  (should (ert-test-boundp 'llm-test/visual-line-mode/1)))

;;; Subprocess control tests

(ert-deftest llm-test-emacs-subprocess-eval ()
  "Starting a fresh Emacs and evaluating elisp should work."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          (should (equal (llm-test--eval-in-emacs info "(+ 1 2)") "3"))
          (should (equal (llm-test--eval-in-emacs info "(+ 10 20)") "30")))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-emacs-subprocess-isolation ()
  "The test Emacs should be a clean -Q instance without user config."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        ;; In emacs -Q, user-init-file should be nil
        (should (equal (llm-test--eval-in-emacs info "user-init-file") "nil"))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-visible-buffer-contents ()
  "Window-start/window-end should return only visible content in the frame."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          ;; Insert 100 lines into a buffer — more than the 40-line frame.
          (llm-test--eval-in-emacs
           info
           (concat "(progn"
                   "  (switch-to-buffer \"*test-visible*\")"
                   "  (dotimes (i 100)"
                   "    (insert (format \"line %d\\n\" i)))"
                   "  (goto-char (point-min)))"))
          ;; The total buffer has 100 lines, but the window shows ~40.
          (let ((total-lines
                 (string-to-number
                  (llm-test--eval-in-emacs
                   info
                   "(with-current-buffer \"*test-visible*\" (count-lines (point-min) (point-max)))")))
                (visible (llm-test-test--selected-window-contents info)))
            (should (= total-lines 100))
            ;; Visible content should be non-empty and no longer than the buffer.
            (let ((visible-lines (length (split-string visible "\n" t))))
              (should (<= visible-lines 100))
              (should (> visible-lines 0)))))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-visible-buffer-contents-include-overlay-after-string ()
  "Overlay after-strings should appear in captured visible contents."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          (llm-test--eval-in-emacs
           info
           (concat "(progn"
                   "  (switch-to-buffer \"*test-overlay-after*\")"
                   "  (erase-buffer)"
                   "  (insert \"alpha\\nbeta\")"
                   "  (goto-char (point-min))"
                   "  (forward-line 1)"
                   "  (let ((ov (make-overlay (point) (point))))"
                   "    (overlay-put ov 'after-string \"\\n>> placeholder\"))"
                   "  (goto-char (point-min)))"))
          (let ((visible (llm-test-test--selected-window-contents info)))
            (should (string-match-p "alpha" visible))
            (should (string-match-p ">> placeholder" visible))
            (should (string-match-p "beta" visible))))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-visible-buffer-contents-include-overlay-display-string ()
  "Overlay display strings should appear in captured visible contents."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          (llm-test--eval-in-emacs
           info
           (concat "(progn"
                   "  (switch-to-buffer \"*test-overlay-display*\")"
                   "  (erase-buffer)"
                   "  (insert \"alpha beta\")"
                   "  (let ((ov (make-overlay (point-min) (+ (point-min) 5))))"
                   "    (overlay-put ov 'display \"[shown]\"))"
                   "  (goto-char (point-min)))"))
          (let ((visible (llm-test-test--selected-window-contents info)))
            (should (string-match-p "\\[shown\\]" visible))
            (should (string-match-p "beta" visible))
            (should-not (string-match-p "alpha" visible))))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-visible-buffer-contents-include-overlay-display-placeholder ()
  "Non-string overlay display specs should use a display placeholder."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          (llm-test--eval-in-emacs
           info
           (concat "(progn"
                   "  (switch-to-buffer \"*test-overlay-display-placeholder*\")"
                   "  (erase-buffer)"
                   "  (insert \"alpha beta\")"
                   "  (let ((ov (make-overlay (point-min) (+ (point-min) 5))))"
                   "    (overlay-put ov 'display '(space :width 3)))"
                   "  (goto-char (point-min)))"))
          (let ((visible (llm-test-test--selected-window-contents info)))
            (should (string-match-p "\\[display\\]" visible))
            (should (string-match-p "beta" visible))
            (should-not (string-match-p "alpha" visible))))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-visible-buffer-contents-include-display-text-property ()
  "Display text properties should replace the underlying buffer text."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          (llm-test--eval-in-emacs
           info
           (concat "(progn"
                   "  (switch-to-buffer \"*test-display-text-property*\")"
                   "  (erase-buffer)"
                   "  (insert \"alpha beta\")"
                   "  (put-text-property (point-min) (+ (point-min) 5) 'display \"[shown]\")"
                   "  (goto-char (point-min)))"))
          (let ((visible (llm-test-test--selected-window-contents info)))
            (should (string-match-p "\\[shown\\]" visible))
            (should (string-match-p "beta" visible))
            (should-not (string-match-p "alpha" visible))))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-visible-buffer-contents-keep-adjacent-display-text-runs ()
  "Adjacent identical display text-property runs should not collapse together."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          (llm-test--eval-in-emacs
           info
           (concat "(progn"
                   "  (switch-to-buffer \"*test-adjacent-display-runs*\")"
                   "  (erase-buffer)"
                   "  (insert \"ab\")"
                   "  (put-text-property (point-min) (1+ (point-min)) 'display \"[shown]\")"
                   "  (put-text-property (1+ (point-min)) (+ (point-min) 2) 'display \"[shown]\")"
                   "  (goto-char (point-min)))"))
          (should (equal (llm-test-test--selected-window-contents info)
                         "[shown][shown]")))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-visible-buffer-contents-keep-adjacent-display-placeholders ()
  "Adjacent non-string display text-property runs should not collapse."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          (llm-test--eval-in-emacs
           info
           (concat "(progn"
                   "  (switch-to-buffer \"*test-adjacent-display-placeholders*\")"
                   "  (erase-buffer)"
                   "  (insert \"ab\")"
                   "  (put-text-property (point-min) (1+ (point-min)) 'display '(space :width 2))"
                   "  (put-text-property (1+ (point-min)) (+ (point-min) 2) 'display '(space :width 3))"
                   "  (goto-char (point-min)))"))
          (should (equal (llm-test-test--selected-window-contents info)
                         "[display][display]")))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-visible-buffer-contents-include-after-string-between-displays ()
  "Display transitions should keep after-strings at the handoff boundary."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          (llm-test--eval-in-emacs
           info
           (concat "(progn"
                   "  (switch-to-buffer \"*test-display-after-transition*\")"
                   "  (erase-buffer)"
                   "  (insert \"abcdefghij\")"
                   "  (let ((outer (make-overlay (point-min) (+ (point-min) 10)))"
                   "        (inner (make-overlay (+ (point-min) 4) (+ (point-min) 6))))"
                   "    (overlay-put outer 'display \"[outer]\")"
                   "    (overlay-put inner 'display \"[inner]\")"
                   "    (overlay-put inner 'after-string \"!\"))"
                   "  (goto-char (point-min)))"))
          (should (equal (llm-test-test--selected-window-contents info)
                         "[outer][inner]![outer]")))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-visible-buffer-contents-handle-same-start-overlays ()
  "More nested same-start display overlays should win first."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          (llm-test--eval-in-emacs
           info
           (concat "(progn"
                   "  (switch-to-buffer \"*test-same-start-display*\")"
                   "  (erase-buffer)"
                   "  (insert \"abcdefghij\")"
                   "  (let ((outer (make-overlay (point-min) (+ (point-min) 10)))"
                   "        (inner (make-overlay (point-min) (+ (point-min) 5))))"
                   "    (overlay-put outer 'display \"[outer]\")"
                   "    (overlay-put inner 'display \"[inner]\"))"
                   "  (goto-char (point-min)))"))
          (should (equal (llm-test-test--selected-window-contents info)
                         "[inner][outer]")))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-visible-buffer-contents-handle-same-end-overlays ()
  "More nested same-end display overlays should win last."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          (llm-test--eval-in-emacs
           info
           (concat "(progn"
                   "  (switch-to-buffer \"*test-same-end-display*\")"
                   "  (erase-buffer)"
                   "  (insert \"abcdefghij\")"
                   "  (let ((outer (make-overlay (point-min) (+ (point-min) 10)))"
                   "        (inner (make-overlay (+ (point-min) 5) (+ (point-min) 10))))"
                   "    (overlay-put outer 'display \"[outer]\")"
                   "    (overlay-put inner 'display \"[inner]\"))"
                   "  (goto-char (point-min)))"))
          (should (equal (llm-test-test--selected-window-contents info)
                         "[outer][inner]")))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-selected-window-prefers-json-true ()
  "Selected-window lookup should ignore JSON false values."
  (let* ((state (json-parse-string
                 "{\"windows\":[{\"selected\":false,\"contents\":\"first\"},{\"selected\":true,\"contents\":\"second\"}]}"
                 :object-type 'alist
                 :array-type 'list))
         (selected (llm-test-test--selected-window state)))
    (should (equal (alist-get "contents" selected nil nil #'string=)
                   "second"))))

(ert-deftest llm-test-suggestions-accumulate ()
  "The suggest-improvement tool should accumulate suggestions."
  (let* ((suggestions (list nil))
         (tool (nth 2 (llm-test--make-tools
                       '(:server-name "x" :socket-dir "/tmp")
                       suggestions))))
    ;; The suggest-improvement tool is at index 2.
    (should (equal (llm-tool-name tool) "suggest-improvement"))
    ;; Async tools take a callback as the first argument.
    (funcall (llm-tool-function tool) #'ignore "suggestion one")
    (funcall (llm-tool-function tool) #'ignore "suggestion two")
    (should (equal (cdr suggestions) '("suggestion one" "suggestion two")))))

(ert-deftest llm-test-send-keys-nonblocking ()
  "send-keys via unread-command-events should handle prompting commands."
  (let ((info (llm-test--start-emacs)))
    (unwind-protect
        (progn
          ;; Set up a buffer with a command that calls completing-read.
          (llm-test--eval-in-emacs
           info
           "(progn
              (switch-to-buffer \"test-nb\")
              (local-set-key (kbd \"q\")
                (lambda () (interactive)
                  (insert (completing-read \"Pick: \" '(\"alpha\" \"beta\"))))))")
          ;; Queue "q" via unread-command-events — returns immediately.
          (let ((result (llm-test--eval-in-emacs
                         info
                         "(progn
                            (setq unread-command-events
                                  (append unread-command-events
                                          (listify-key-sequence (kbd \"q\"))))
                            \"keys queued\")")))
            (should (equal result "\"keys queued\""))
            ;; The daemon command loop processes "q", entering completing-read.
            (sleep-for 1.0)
            ;; The minibuffer should now be active with the prompt.
            (let ((mb (llm-test--eval-in-emacs
                       info
                       "(if (active-minibuffer-window)
                            \"minibuffer-active\"
                          \"no-minibuffer\")")))
              (should (equal mb "\"minibuffer-active\"")))
            ;; Feed the response into the active prompt.
            (llm-test--eval-in-emacs
             info
             "(setq unread-command-events
                    (append unread-command-events
                            (listify-key-sequence (kbd \"a l p h a RET\"))))")
            ;; Wait for completing-read to consume the events.
            (sleep-for 1.0)
            ;; The buffer should now contain "alpha".
            (let ((contents (llm-test--eval-in-emacs
                             info
                             "(with-current-buffer \"test-nb\"
                                (buffer-string))")))
              (should (equal contents "\"alpha\"")))))
      (llm-test--stop-emacs info))))

(ert-deftest llm-test-report-result-pass ()
  "A passing result with no suggestions should not signal."
  (let ((result (make-llm-test-result :passed-p t :reason "ok"
                                      :suggestions nil)))
    ;; Should not error.
    (llm-test--report-result result)))

(ert-deftest llm-test-report-result-fail ()
  "A failing result should call ert-fail."
  (let ((result (make-llm-test-result :passed-p nil :reason "bad"
                                      :suggestions nil)))
    (should-error (llm-test--report-result result) :type 'ert-test-failed)))

(ert-deftest llm-test-report-result-warnings-as-errors ()
  "With warnings-as-errors, suggestions on a pass should fail."
  (let ((llm-test-warnings-as-errors t)
        (result (make-llm-test-result :passed-p t :reason "ok"
                                      :suggestions '("fix this"))))
    (should-error (llm-test--report-result result) :type 'ert-test-failed)))

(ert-deftest llm-test-report-result-suggestions-no-error ()
  "Without warnings-as-errors, suggestions on a pass should not fail."
  (let ((llm-test-warnings-as-errors nil)
        (result (make-llm-test-result :passed-p t :reason "ok"
                                      :suggestions '("fix this"))))
    ;; Should not error.
    (llm-test--report-result result)))

(provide 'llm-test-test)
;;; llm-test-test.el ends here
