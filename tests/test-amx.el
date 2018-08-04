;;; -*- lexical-binding: t -*-

(require 'smex)
(require 'amx)
(require 'buttercup)
(require 'cl-lib)
(require 'with-simulated-input)
;; Need to load these to ensure certian functions are not autoloads
(require 'help-fns)
(require 'find-func)

(smex-initialize)

(defun test-save-custom-vars (vars)
  (cl-loop
   for var in vars
   if (not (custom-variable-p var))
   do (error "Variable `%s' is not a customizable variable" var)
   for curval = (symbol-value var)
   for stdval = (eval (car (get var 'standard-value)))
   do
   (progn
     ;; Save the current value
     (put var 'test-saved-value curval)
     ;; Set it to the standard value, using it's custom setter
     ;; function
     (customize-set-variable var stdval))))

(defun test-restore-custom-vars (vars)
  (cl-loop
   for var in vars
   for savedval = (get var 'test-saved-value)
   do
   (progn
     ;; Set it to the saved value, using it's custom setter function
     (customize-set-variable var savedval)
     ;; Delete the saved value from the symbol plist
     (put var 'test-saved-value nil))))

(defun key-sequences-equal (k1 k2)
  "Return non-nil if K1 and K2 represent the same key sequence.

If K1 or K2 are strings, they are converted to internal
representation using `kbd', and the results are compared. Hence
`C-M-a', `M-C-a', and `[134217729]' should all be considered
equal."
  (let ((k1 (if (stringp k1)
                (kbd k1)
              k1))
        (k2 (if (stringp k2)
                (kbd k2)
              k2)))
    (equal k1 k2)))

(defun canonicalize-key-sequence (k)
  (key-description (kbd k)))

(cl-defun amx-completing-read-return-first-choice
    (choices &key initial-input predicate)
  (car (all-completions (or initial-input "") choices predicate)))

;; Create some commands with a known names
(defun my-temp-command ()
  (interactive)
  (message "Ran my-temp-command"))

(defun my-temp-command-2 ()
  (interactive)
  (message "Ran my-temp-command-2"))

(describe "The amx package"

  :var (last-choice-list
        orig-amx-completing-read)
  (before-all
    ;; Wrapper that saves the choice list
    (setq orig-amx-completing-read (symbol-function 'amx-completing-read))
    (spy-on 'amx-completing-read :and-call-fake
            ;; Save the choices list and then call original
            (cl-function
             (lambda (choices &rest morekeys &key predicate &allow-other-keys)
               (setq last-choice-list (all-completions "" choices predicate))
               (apply orig-amx-completing-read choices
                      :predicate predicate
                      morekeys)))))

  ;; Reset all of these variables to their standard values before each
  ;; test
  (before-each
    ;; Suppress messages
    (spy-on 'message)
    (test-save-custom-vars
     '(amx-mode
       amx-auto-update-interval
       amx-save-file
       amx-history-length
       amx-show-key-bindings
       amx-ignored-command-matchers
       amx-backend
       smex-save-file))
    ;; Don't save anything to disk during testing
    (setq amx-save-file nil)
    ;; Start each test with amx caches fully updated
    (amx-idle-update t))

  ;; Restore the saved value after each test
  (after-each
    (test-restore-custom-vars
     '(amx-mode
       amx-auto-update-interval
       amx-save-file
       amx-history-length
       amx-show-key-bindings
       amx-ignored-command-matchers
       amx-backend
       smex-save-file)))

  (it "should execute the selected command"
    (spy-on 'my-temp-command)
    (with-simulated-input "my-temp-command RET"
      (amx-read-and-run '(my-temp-command)))
    (expect 'my-temp-command
            :to-have-been-called))

  (it "should not allow setting a backend without loading the required feature"
    ;; Override `require' to return nil to prevent loading of new features
    (spy-on 'require :and-return-value nil)
    (expect
     (customize-set-variable 'amx-backend 'ido)
     :to-throw)
    (expect
     (customize-set-variable 'amx-backend 'ivy)
     :to-throw))

  (it "should activate `amx-map' while running amx"

    (spy-on 'describe-function)
    (spy-on 'pop-to-buffer)
    (spy-on 'where-is)
    (spy-on 'find-function)
    (with-simulated-input "my-temp-command C-h f"
      (amx-read-and-run '(my-temp-command)))
    (expect 'describe-function
            :to-have-been-called)
    (with-simulated-input "my-temp-command C-h w"
      (amx-read-and-run '(my-temp-command)))
    (expect 'where-is
            :to-have-been-called)
    (with-simulated-input "my-temp-command M-."
      (amx-read-and-run '(my-temp-command)))
    (expect 'find-function
            :to-have-been-called))

  (describe "standard backend"

    (before-each
      (customize-set-variable 'amx-backend 'standard)
      (spy-on 'completing-read-default :and-call-through)
      (spy-on 'completing-read :and-call-through))

    (it "should call `completing-read-default' and not `completing-read'"
      (expect
       (with-simulated-input "ignore RET"
         (amx-completing-read '("ignore")))
       :to-equal "ignore")
      (expect 'completing-read-default
              :to-have-been-called)
      (expect 'completing-read :not
              :to-have-been-called)))

  (describe "ido backend"

    (before-each
      (customize-set-variable 'amx-backend 'ido)
      (spy-on 'ido-completing-read+ :and-call-through))

    (it "should load `ido-completing-read+' when selected"
      (customize-set-variable 'amx-backend 'ido)
      (expect (featurep 'ido-completing-read+)))

    (it "should call `ido-completing-read+'"
      (expect
       (with-simulated-input "ignore RET"
         (amx-completing-read '("ignore")))
       :to-equal "ignore")
      (expect 'ido-completing-read+
              :to-have-been-called)))

  (describe "ivy backend"

    (before-each
      (customize-set-variable 'amx-backend 'ivy)
      (spy-on 'ivy-read :and-call-through))

    (it "should load `ivy' when selected"
      (customize-set-variable 'amx-backend 'ivy)
      (expect (featurep 'ivy)))

    (it "should call `ivy-read'"
      (expect
       (with-simulated-input "ignore RET"
         (amx-completing-read '("ignore")))
       :to-equal "ignore")
      (expect 'ivy-read
              :to-have-been-called)))

  (describe "auto backend"

    (before-each
      (customize-set-variable 'amx-backend 'auto)
      ;; Pre-load features so we can spy on their functions
      (require 'ido-completing-read+)
      (require 'ivy)
      ;; Reset all of these modes to their standard values
      ;; before each test
      (test-save-custom-vars '(ido-mode ivy-mode))
      ;; Start with both modes off
      (ido-mode 0)
      (ivy-mode 0)
      (cl-loop
       for fun in
       '(completing-read-default ido-completing-read+ ivy-read)
       do (spy-on fun :and-return-value "ignore")))

    ;; Restore the saved value after each test
    (after-each
      (test-restore-custom-vars '(ido-mode ivy-mode)))

    (it "should normally use standard completion"
      (amx-completing-read '("ignore"))
      (expect 'completing-read-default
              :to-have-been-called))

    (it "should use ido completion when `ido-mode' or `ido-ubiquitous-mode' are enabled"
      (ido-mode 1)
      (amx-completing-read '("ignore"))
      (expect 'ido-completing-read+
              :to-have-been-called))

    (it "should use ivy completion when `ivy-mode' is enabled"
      (ivy-mode 1)
      (amx-completing-read '("ignore"))
      (expect 'ivy-read
              :to-have-been-called)))

  (describe "with `amx-show-key-bindings'"

    :var (orig-local-map
          orig-amx-completing-read
          my-key-sequence
          temp-map)

    (before-each
      ;; Save the information needed to undo everything
      (setq orig-local-map (current-local-map)
            temp-map (make-sparse-keymap)
            my-key-sequence (canonicalize-key-sequence "C-M-A-H-s-a"))
      ;; Reversibly add a custom binding to the local map
      (define-key temp-map (kbd my-key-sequence) 'my-temp-command)
      (use-local-map (make-composed-keymap temp-map orig-local-map))
      ;; Ido lets us select entries using any substring
      (customize-set-variable 'amx-backend 'ido)
      (customize-set-variable 'amx-show-key-bindings t)
      (spy-on 'amx-augment-commands-with-keybinds :and-call-through)
      (spy-on 'amx-make-keybind-hash :and-call-through)
      ;; Don't actually execute selected commands
      (spy-on 'execute-extended-command))

    (after-each
      ;; Undo the overridden local map
      (use-local-map orig-local-map))

    (it "should add key bindings successfully"
      (expect
       (cl-some
        (apply-partially 's-contains? my-key-sequence)
        (amx-augment-commands-with-keybinds '(my-temp-command) (amx-make-keybind-hash)))))

    (it "should show key bindings and update the keybind hash when enabled"
      (with-simulated-input "RET"
        (amx-read-and-run amx-cache "my-temp-command"))
      (expect 'execute-extended-command
              :to-have-been-called-with nil "my-temp-command")
      (expect 'amx-augment-commands-with-keybinds
              :to-have-been-called)
      (expect (cl-some (apply-partially 's-contains? my-key-sequence)
                       last-choice-list)))

    (it "should allow completion on key bindings"
      (with-simulated-input "RET"
        (amx-read-and-run amx-cache my-key-sequence))
      (expect 'execute-extended-command
              :to-have-been-called-with nil "my-temp-command"))

    (it "should still consider the command without its binding as a match"
      (customize-set-variable 'amx-backend 'standard)
      ;; Should fail with incomplete completion
      (with-simulated-input "my-temp-command TAB RET"
        (amx-read-and-run amx-cache))
      (expect 'execute-extended-command
              :to-have-been-called-with nil "my-temp-command"))

    (it "should not show key bindings or update the keybind hash when disabled"
      (setq amx-show-key-bindings nil)
      (with-simulated-input "RET"
        (amx-read-and-run amx-cache "my-temp-command"))
      (expect 'execute-extended-command
              :to-have-been-called-with nil "my-temp-command")
      (expect 'amx-augment-commands-with-keybinds
              :not :to-have-been-called)
      (expect 'amx-make-keybind-hash
              :not :to-have-been-called)
      (expect (not (cl-some (apply-partially 's-contains? my-key-sequence)
                            last-choice-list))))

    (it "should use `amx-origin-buffer' instead of current buffer when looking up key binds"
      (setq amx-show-key-bindings t)
      (with-temp-buffer
        (let ((amx-origin-buffer (current-buffer)))
          ;; Now my-temp-command is not bound in active maps, so its
          ;; key binding should not show up in completions
          (with-simulated-input "RET"
            (amx-read-and-run amx-cache "my-temp-command"))
          (expect 'execute-extended-command
                  :to-have-been-called-with nil "my-temp-command")
          (expect (not (cl-some (apply-partially 's-contains? my-key-sequence)
                                last-choice-list))))))

    (it "should update the keybind hash after switching buffers"
      (setq amx-show-key-bindings t)
      (with-temp-buffer
        (let ((amx-origin-buffer (current-buffer)))
          (with-simulated-input "RET"
            (amx-read-and-run amx-cache "my-temp-command"))
          (expect 'execute-extended-command
                  :to-have-been-called-with nil "my-temp-command")
          (expect 'amx-augment-commands-with-keybinds
                  :to-have-been-called)))))

  (describe "auto-update functionality"

    :var (amx-last-update-time)

    (before-each
      (spy-on 'amx-idle-update :and-call-through)
      (spy-on 'amx-update-if-needed :and-call-through)
      (spy-on 'amx-detect-new-commands :and-call-through)
      (spy-on 'amx-update :and-call-through))

    (it "should not force an update on the short idle timer"
      ;; Trigger a short idle update
      (amx-idle-update)
      (expect 'amx-idle-update
              :to-have-been-called)
      (expect 'amx-update-if-needed
              :to-have-been-called)
      (expect 'amx-detect-new-commands
              :not :to-have-been-called)
      (expect 'amx-update
              :not :to-have-been-called))

    (it "should do an update on the short idle timer when needed"
      (amx-post-eval-force-update)
      ;; No update runs yet
      (expect 'amx-idle-update
              :not :to-have-been-called)
      ;; Trigger a short idle update
      (amx-idle-update)
      (expect 'amx-idle-update
              :to-have-been-called)
      (expect 'amx-update-if-needed
              :to-have-been-called)
      (expect 'amx-detect-new-commands
              :not :to-have-been-called)
      (expect 'amx-update
              :to-have-been-called))

    (it "should force an command recount when idle for `auto-update-interval'"
      (customize-set-variable 'amx-auto-update-interval 60)
      ;; Pretend that amx is due for an update
      (setq amx-last-update-time
            (time-subtract (current-time)
                           (seconds-to-time
                            (* 60 (1+ amx-auto-update-interval)))))
      (amx-idle-update)
      (expect 'amx-idle-update
              :to-have-been-called)
      (expect 'amx-update-if-needed
              :to-have-been-called)
      (expect 'amx-detect-new-commands
              :to-have-been-called))

    (it "should cancel the long-update timer when `auto-update-interval' is nil"
      (customize-set-variable 'amx-auto-update-interval 60)
      (expect amx-long-idle-update-timer
              :to-be-truthy)
      (customize-set-variable 'amx-auto-update-interval nil)
      (expect amx-long-idle-update-timer
              :not :to-be-truthy)))

  (describe "with `amx-save-file'"

    :var (orig-temporary-file-directory
          temporary-file-directory
          orig-init-file-user
          init-file-user
          amx-temp-commands
          saved-amx-history
          saved-amx-data
          old-amx-save-file)

    (before-each
      ;; Pretend that we're not running under "emacs -Q" by setting
      ;; `init-file-user' non-nil
      (setq orig-init-file-user init-file-user
            init-file-user (or init-file-user "nobody"))
      ;; Set up a private temporary directory for each test
      (setq orig-temporary-file-directory temporary-file-directory
            temporary-file-directory (make-temp-file "amx-test-temp-" t)))

    (after-each
      ;; Delete the test-private temp dir
      (delete-directory temporary-file-directory t nil)
      (setq temporary-file-directory orig-temporary-file-directory
            init-file-user orig-init-file-user))

    (it "should be able to save to and load amx data from a file"
      (customize-set-variable 'amx-save-file (make-temp-file "amx-items-temp-"))
      (amx-save-to-file)
      (setq saved-amx-history amx-history
            amx-history nil
            saved-amx-data amx-data
            amx-data nil)
      (amx-load-save-file)
      (expect amx-history
              :to-equal saved-amx-history)
      (expect amx-data
              :to-equal saved-amx-data))

    (it "should handle trying to load a nonexistent file"
      (customize-set-variable
       'amx-save-file
       (make-temp-name (expand-file-name
                        "amx-items-temp-"
                        temporary-file-directory)))
      (when (file-exists-p amx-save-file)
        (delete-file amx-save-file nil))
      (assume (not (file-exists-p amx-save-file)))
      (expect (amx-load-save-file)
              :not :to-throw))

    (it "should not save when `init-file-user' or `amx-save-file' are nil"
      (customize-set-variable
       'amx-save-file
       (make-temp-name
        (expand-file-name "amx-items-temp-"
                          temporary-file-directory)))
      (when (file-exists-p amx-save-file)
        (delete-file amx-save-file nil))
      (assume (not (file-exists-p amx-save-file)))
      (cl-letf ((init-file-user nil)
                ((symbol-function 'display-warning)
                 (symbol-function 'ignore)))
        (amx-save-to-file))
      (let ((amx-save-file nil))
        (amx-save-to-file))
      (expect (not (file-exists-p amx-save-file))))

    (it "should load from `smex-save-file' if `amx-save-file' does not exist"
      (customize-set-variable
       'amx-save-file
       (make-temp-name
        (expand-file-name "amx-items-temp-"
                          temporary-file-directory)))
      (customize-set-variable
       'smex-save-file
       (make-temp-name
        (expand-file-name "smex-items-temp-"
                          temporary-file-directory)))
      ;; Save data to smex save file
      (let ((amx-save-file smex-save-file))
        (amx-save-to-file))
      (setq saved-amx-history amx-history
            amx-history nil
            saved-amx-data amx-data
            amx-data nil)
      (assume (not (file-exists-p amx-save-file)))
      ;; Should load from smex save file
      (amx-load-save-file)
      (expect amx-history
              :to-equal saved-amx-history)
      (expect amx-data
              :to-equal saved-amx-data))

    (describe "changing while Emacs is running"

      (it "should copy the old save file if the new one doesn't exist already"
        (customize-set-variable
         'amx-save-file
         (make-temp-name
          (expand-file-name "amx-items-temp-"
                            temporary-file-directory)))
        (amx-save-to-file)
        (expect (file-exists-p amx-save-file))
        (let ((old-save-file amx-save-file)
              (new-save-file (make-temp-name
                              (expand-file-name "amx-items-renamed-"
                                                temporary-file-directory))))
          (expect (not (file-exists-p new-save-file)))
          ;; Switch to the new file
          (customize-set-variable 'amx-save-file new-save-file)
          (expect (file-exists-p new-save-file))))

      (it "should reinitialize Amx from an already existing save file"
        (customize-set-variable
         'amx-save-file
         (make-temp-name
          (expand-file-name "amx-items-temp-"
                            temporary-file-directory)))
        (amx-save-to-file)
        (setq saved-amx-history amx-history
              amx-history nil
              saved-amx-data amx-data
              amx-data nil)
        (let ((old-save-file amx-save-file)
              (new-save-file (expand-file-name
                              (make-temp-name
                               (expand-file-name "amx-items-renamed-"
                                                 temporary-file-directory))
                              temporary-file-directory)))
          (expect (not (file-exists-p new-save-file)))
          (rename-file old-save-file new-save-file)
          ;; Switch to the new file
          (customize-set-variable 'amx-save-file new-save-file)
          ;; Switching files should auto-initialize from the new file,
          ;; restoring these variables
          (expect amx-history
                  :to-equal saved-amx-history)
          (expect amx-data
                  :to-equal saved-amx-data)))))

  (describe "with `amx-ignored-command-matchers'"

    (before-each
      ;; Don't actually execute selected commands
      (spy-on 'execute-extended-command))

    (it "should ignore commands matching a regexp"
      (add-to-list 'amx-ignored-command-matchers
                   "\\`my-temp-command-2\\'")
      (with-simulated-input "my-temp-command RET"
        (amx-read-and-run '(my-temp-command my-temp-command-2)))
      (expect last-choice-list
              :not :to-contain "my-temp-command-2"))

    (it "should ignore commands matching a function"
      (add-to-list 'amx-ignored-command-matchers
                   (lambda (cmd) (string= "my-temp-command-2" (format "%s" cmd))))
      (with-simulated-input "my-temp-command RET"
        (amx-read-and-run '(my-temp-command my-temp-command-2)))
      (expect last-choice-list
              :not :to-contain "my-temp-command-2"))

    (it "should ignore commands explicitly marked as ignored by `amx-ignore-command'"
      (unwind-protect
          (progn
            (amx-ignore-command "my-temp-command-2")
            (with-simulated-input "my-temp-command RET"
              (amx-read-and-run '(my-temp-command my-temp-command-2)))
            (expect last-choice-list
                    :not :to-contain "my-temp-command-2"))
        (amx-unignore-command "my-temp-command-2")))

    (it "should still allow executing ignored commands"
      (add-to-list 'amx-ignored-command-matchers
                   "\\`my-temp-command-2\\'")
      (with-simulated-input "my-temp-command-2 RET"
        (amx-read-and-run '(my-temp-command my-temp-command-2)))
      (expect 'execute-extended-command
              :to-have-been-called-with nil "my-temp-command-2")))

  (describe "`amx-mode'"

    (before-each
      (amx-mode 1))

    (it "should replace M-x when enabled"
      (expect (key-binding [remap execute-extended-command])
              :to-be 'amx))

    (it "should not replace M-x when disabled"
      (amx-mode 0)
      (expect (key-binding [remap execute-extended-command])
              :not :to-be 'amx))))

;;; test-amx.el ends here
