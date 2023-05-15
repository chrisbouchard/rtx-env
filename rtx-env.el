;;; rtx-env.el --- rtx environment management  -*- lexical-binding: t; -*-

;; Copyright © 2023 Chris Bouchard
;; Copyright © 2022–2023 Augusto Stoffel
;; Copyright © 2020–2023 Steve Purcell

;; Author: Chris Bouchard <chris@upliftinglemma.net>
;; URL: https://github.com/chrisbouchard/rtx-env
;; Created: 2023-04-23
;; Keywords: processes, tools
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.1"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; rtx-env is a library for managing your Emacs environment using rtx.

;; This library is based on two other libraries for managing buffer-local state:
;;
;;   * https://github.com/purcell/envrc
;;   * https://github.com/astoff/buffer-env
;;

;;; Code:

(require 'cl-lib)
(require 'compat)


;;; Customization

(defgroup rtx-env nil
  "rtx environment management"
  :group 'environment)

(defcustom rtx-env-add-advice t
  "Enable rtx-env to add advice for `setenv' to additionally update the default
value of `process-environment', even when there is a local binding.

NOTE: rtx-env adds advice when the package is loaded. If you change this value
after rtx-env is loaded, you'll need to reload it."
  :type '(radio (const :tag "Don't add advice" nil)
                (other :tag "Add advice" t)))

(defcustom rtx-env-command "rtx exec -- env --null"
  "Command to print the rtx environment as NUL-terminated NAME=VALUE pairs, like
`env --null'. (See the man page `env(1)' for reference.)"
  :type '(string :tag "Command string")
  :risky t)

;; TODO: Unused
(defcustom rtx-env-config-environment nil
  "Config environment to use with rtx"
  :type '(choice (string :tag "Config environment name")
                 (const :tag "Unset" nil))
  :risky t)

(defcustom rtx-env-default-directory user-home-directory
  "Directory to use when running global rtx commands. If nil, use the current
buffer's `default-directory'."
  :type '(choice (directory :tag "Directory")
                 (const :tag "Current Buffer" nil))
  :risky t)

(defcustom rtx-env-debug nil
  "Enable rtx-env debug logging if non-nil."
  :type '(radio (const :tag "Disable debug logging" nil)
                (other :tag "Enable debug logging" t)))

(defcustom rtx-env-ignored-variables nil
  "List of variable names to ignore"
  :type '(repeat :tag "Environment variable names" string))

(defcustom rtx-env-manage-exec-path t
  "Enable rtx-env management of `exec-path' based on environment variable
`PATH'."
  :type '(radio (const :tag "Don't manage `exec-path'" nil)
                (other :tag "Manage `exec-path'" t)))

;; TODO: Unused
(defcustom rtx-env-tool-overrides nil
  "List of tool version overrides"
  :type '(alist :key-type (string :tag "Tool name")
                :value-type (string :tag "Tool version")))

(defcustom rtx-env-update-hook nil
  "Normal hook run after `rtx-env-update' updates the environment."
  :type 'hook
  :risky t)


;;; Commands

;;;###autoload
(cl-defun rtx-env-update (&optional (buffer (current-buffer)))
  "Update BUFFER's environment using rtx.

If `rtx-env-local-mode' is active in BUFFER, this will only update that buffer's
environment. Otherwise, it will update the global environment. BUFFER defaults
to the current buffer.

If `rtx-env-manage-exec-path' is non-nil (the default), additionally update
`exec-path' based on the environment variable `PATH'."
  (interactive "bBuffer:")
  ;; TODO: Move some of this logic into private functions.
  (with-current-buffer buffer
    (rtx-env--message :debug "Updating environment in buffer %s" (buffer-name))
    (let ((callback (lambda (environment)
                      (rtx-env--update-emacs-environment environment)
                      (run-hooks 'rtx-env-update-hook)
                      (rtx-env--message :debug "Finished updating environment"))))
      (rtx-env--read-environment-from-command callback))))


;;; Minor Modes

;;;###autoload
(define-minor-mode rtx-env-local-mode
  "Minor mode to automatically manage buffer-local environments using rtx.

Enabling this mode makes `process-environment' and optionally `exec-path'
buffer-local variables, and then updates them using `rtx-env-update'.

Disabling this mode removes the buffer-local bindings for `process-environment'
and optionally `exec-path'.

`exec-path' is only modified if `rtx-env-manage-exec-path' is non-nil."
  :init-value nil
  :lighter "rtx"
  (if rtx-env-local-mode
      (progn
        (rtx-env--make-local-variables)
        (rtx-env-update))
    (rtx-env--kill-local-variables)))

;;;###autoload
(define-globalized-minor-mode rtx-env-global-local-mode rtx-env-local-mode
  (lambda ()
    (unless (or (minibufferp) (file-remote-p default-directory))
      (rtx-env-local-mode 1))))


;;; Implementation

(defvar rtx-env--skip-updating-default-process-environment nil
  "Dynamic variable to make `rtx-env--default-process-environment-advice' act on
the buffer's local `process-environment' binding (if it exists), rather than
always on the default value.")


;; Local Variable Management

(defun rtx-env--make-local-variables ()
  "Make `process-environment' and optionally `exec-path' buffer-local variables.

`exec-path' is only made local if `rtx-env-manage-exec-path' is non-nil."
  (make-local-variable 'process-environment)
  (when rtx-env-manage-exec-path
    (make-local-variable 'exec-path)))

(defun rtx-env--kill-local-variables ()
  "Remove buffer-local bindings for `process-environment' and optionally
`exec-path'.

The local binding for `exec-path' is only removed if `rtx-env-manage-exec-path'
is non-nil."
  (kill-local-variable 'process-environment)
  (when rtx-env-manage-exec-path
    (kill-local-variable 'exec-path)))


;; Update Environment

(defun rtx-env--update-emacs-environment (environment)
  "Update `process-environment' and optionally `exec-path' with the environment
variables in ENVIRONMENT.

ENVIRONMENT is a list of (NAME . VALUE) pairs.

If `rtx-env-local-mode' is active, reset `process-environment' to a copy of the
default value before updating. `exec-path' is only updated if
`rtx-env-manage-exec-path' is non-nil."
  ;; TODO: Add some debug logging.
  (when rtx-env-local-mode
    (setq process-environment
          (copy-sequence (default-value 'process-environment))))
  (let ((rtx-env--skip-updating-default-process-environment t))
    (cl-loop for (name . value) in environment
             do (setenv name value))
    (when rtx-env-manage-exec-path
      (setq exec-path (parse-colon-path (getenv "PATH"))))))


;; RTX Asynchronous Process

(defun rtx-env--read-environment-from-command (callback)
  "Return the environment `rtx-env-rtx-env-command' as a list of (NAME . VALUE)
pairs."
  (let ((command (split-string-shell-command rtx-env-command))
        (default-directory (rtx-env--default-directory))
        (stdout (generate-new-buffer " rtx stdout" 'inhibit-hooks))
        (stderr (generate-new-buffer " rtx stderr" 'inhibit-hooks))
        (sentinel (apply-partially #'rtx-env--command-sentinel callback)))
    (make-process :name "rtx-env-command-stdout"
                  :buffer stdout
                  :command command
                  :noquery t
                  :sentinel sentinel
                  :stderr (make-pipe-process :name "rtx-env-command-stderr"
                                             :buffer stderr
                                             :noquery t))))

(defmacro rtx-env--let-unwind-kill-buffers (buffer-list &rest body)
  "Bind variables to buffers just like `let*', and kill those buffers at the end
of the scope.

(fn ((SYMBOL BUFFER)...) BODY...)"
  (declare (indent 1))
  (cl-loop for (symbol bufferform) in buffer-list
           collect symbol into symbols
           append (list symbol bufferform) into assignments
           collect `(when ,symbol (kill-buffer ,symbol)) into kill-buffers
           finally return `(let (,@symbols)
                             (unwind-protect
                                 (progn
                                   (setq ,@assignments)
                                   ,@body)
                               ,@kill-buffers))))

(defun rtx-env--command-sentinel (callback process event)
  "Handle events from the process running `rtx-env-command'."
  ;; Nothing to do until the process exits. The default filter functions will
  ;; write output into the stdout and stderr buffers.
  (unless (process-live-p process)
    (rtx-env--let-unwind-kill-buffers
        ((stdout (process-buffer process))
         (stderr (process-buffer (process-contact process :stderr))))
      (rtx-env--insert-buffer-substring :debug stdout)
      (rtx-env--insert-buffer-substring :error stderr)
      ;; `process-exit-status' should return non-zero if the process exited
      ;; abnormally -- either the process's status code, or the PID of the
      ;; process that killed it.
      (if (not (equal 0 (process-exit-status process)))
          (rtx-env--message :error "%s" event)
        (rtx-env--message :debug "%s" event)
        (let ((environment (rtx-env--parse-environment stdout)))
          (funcall callback environment))))))

(defun rtx-env--default-directory ()
  "Return the value to use as `default-directory' to run the rtx command."
  (or (unless rtx-env-local-mode
        rtx-env-default-directory)
      default-directory))


;; Environment Parsing

(defun rtx-env--parse-environment (buffer)
  "Parse the contents of BUFFER into a list of (NAME . VALUE) pairs.

BUFFER should contain NUL-terminated `NAME=VALUE' pairs."
  (with-current-buffer buffer
    (cl-loop for (name . value) = (rtx-env--parse-next-env-pair)
             while name
             unless (member name rtx-env-ignored-variables)
             collect `(,name . ,value))))

(defconst rtx-env--env-line-regex
  (rx point (group (*? anything)) "=" (group (*? anything)) "\0")
  "Regexp to parse an environment variable from a NUL-separated environment.")

(defun rtx-env--parse-next-env-pair ()
  "Parse the next environment variable from the current buffer. Returns a pair
(NAME . VALUE)."
  (when (re-search-forward rtx-env--env-line-regex nil t)
    ;; The regex matches the name in group 1, and the value in group 2.
    `(,(match-string 1) . ,(match-string 2))))


;; Advice

(defmacro rtx-env--with-default-value (variable &rest body)
  "Evaluate BODY with VARIABLE as its default value (ignoring any local
bindings).

If BODY updates the value of VARIABLE, that value will be set as the new default
value."
  (declare (indent 1))
  (let ((new-value (gensym))
        (return-value (gensym)))
    `(cl-multiple-value-bind (,return-value ,new-value)
         ;; Any updates to VARIABLE inside the `let' will update that binding,
         ;; not the global variable. To get around that, we return both the
         ;; value of BODY and the final value of VARIABLE, so that later we can
         ;; globally update VARIABLE.
         (let ((,variable (default-value ',variable)))
           (cl-values (progn ,@body) ,variable))
       (setq-default ,variable ,new-value)
       ,return-value)))

(defun rtx-env--default-process-environment-advice (advised &rest args)
  "`:around' advice for `setenv' to additionally update the default value of
`process-environment', even when there is a local binding.

If `rtx-env--skip-updating-default-process-environment' is non-nil, only update
the current binding (regardless if it's buffer-local or global), and don't
additionally update the default binding."
  ;; First apply ADVISED with whatever the current binding for
  ;; `process-environment' is.
  (apply advised args)
  ;; Then, if we are allowed to update the default value and
  ;; `process-environment' is a local binding, apply ADVISED with the default
  ;; value. (If `process-environment' is *not* a local binding, then we already
  ;; updated the default value the first time we applied ADVISED.)
  (when (and (not rtx-env--skip-updating-default-process-environment)
             (local-variable-p 'process-environment))
    (rtx-env--with-default-value process-environment
      (apply advised args))))

(defun rtx-env--add-advice ()
  (advice-add 'setenv :around #'rtx-env--default-process-environment-advice))

(defun rtx-env--remove-advice ()
  (advice-remove 'setenv #'rtx-env--default-process-environment-advice))


;; Logging

(defconst rtx-env--special-buffer-info
  '((:debug . (:name "*rtx-debug*" :enabled rtx-env-debug))
    (:error . (:name "*rtx-error*" :enabled t)))
  "Association list describing the rtx-env special buffers.

Each entry maps a special buffer symbol to a property list with the following
properties:

* `:name' -- the buffer name
* `:enabled' -- whether the special buffer is allowed to be used")

(defmacro rtx-env--with-special-buffer (target &rest body)
  "Evaluate BODY in the special buffer specified by TARGET, creating a new one
if needed. The buffer is mapped using"
  (declare (indent 1))
  (let* ((buffer (gensym)))
    `(when-let ((,buffer (rtx-env--get-special-buffer-create ,target)))
       (with-current-buffer ,buffer
         ,@body))))

(defun rtx-env--get-special-buffer-create (target)
  "Return the special buffer TARGET, creating a new buffer if needed. If the
buffer is not enabled, return nil."
  (when-let* ((buffer-info (alist-get target rtx-env--special-buffer-info))
              (buffer-enabled (eval (plist-get :enabled buffer-info)))
              (buffer-name (eval (plist-get :name buffer-info)))
              (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      ;; Disable recording undo information
      (setq-local buffer-undo-list t)
      (special-mode)
      (read-only-mode)
      (rtx-env-local-mode -1))
    buffer))

(defmacro rtx-env--writing-at-end-of-buffer (&rest body)
  "Evaluate BODY at the end of buffer NAME, in `special-mode'.
To avoid confusion, `rtx-env-local-mode' is explicitly disabled in the buffer."
  (declare (indent 0))
  `(save-excursion
     (let ((inhibit-read-only t))
       (goto-char (point-max))
       ,@body)))


(defun rtx-env--message (target format-string &rest arguments)
  "Write a message to special buffer TARGET. FORMAT-STRING and ARGUMENTS are the
same as for the `format-message' function."
  (rtx-env--with-special-buffer target
    (rtx-env--writing-at-end-of-buffer
      (insert (apply #'format-message format-string arguments))
      (newline)))
  nil)

(defun rtx-env--insert-buffer-substring (target from-buffer-or-name &optional start end)
  "Insert a portion of the buffer FROM-BUFFER-OR-NAME into special buffer TARGET
before point. FROM-BUFFER-OR-NAME, START, and END are the same as for the
`insert-buffer-substring' function."
  (rtx-env--with-special-buffer target
    (rtx-env--writing-at-end-of-buffer
      (insert-buffer-substring from-buffer-or-name start end)))
  nil)


(defun rtx-env--alist-get-str (key alist)
  "Find the first element of ALIST whose `car' `string='s KEY and return its `cdr'."
  (alist-get key alist nil nil #'string=))


;;; Setup

;; When reloading, add or remove advice to match the `rtx-env-add-advice' custom
;; variable.
(if rtx-env-add-advice
    (rtx-env--add-advice)
  (rtx-env--remove-advice))


;;; Unloading

(defun rtx-env-unload-function ()
  "Tear down the rtx-env feature prior to unloading."
  ;; I don't think unloading removes advice, so let's be safe.
  (rtx-env--remove-advice)
  ;; Signal that we want the default unload behavior too.
  nil)


;;; Provides

(provide 'rtx-env)

;;; rtx-env.el ends here
