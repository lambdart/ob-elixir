;;; ob-elixir.el --- Org Babel functions for Elixir evaluation -*- lexical-binding: t; -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/ob-elixir
;; Keywords: org-mode ob-elixir elixir iex
;; Version: 0.0.3 Alpha
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; Org Babel support for evaluating Elixir source code
;; blocks.
;;
;; To use ob-elixir in an org-babel source block,
;; the Elixir language must be enabled in the
;; custom org-babel-load-languages alist:
;;
;; (add-to-list 'org-babel-tangle-lang-exts '("elixir" . "iex"))
;;
;; Alternatively, running the following snippet during
;; Emacs initialization (or latter):
;;
;; (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((emacs-lisp . t)
;;     (elixir . t)))
;;
;;; Code:

(require 'ob)
(require 'ob-comint)

(defcustom org-babel-elixir-program "iex"
  "Name of the program that will execute the Elixir source code block."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-elixir-mode 'elixir-mode
  "Elixir major mode."
  :group 'org-babel
  :type 'symbol)

(defcustom org-babel-elixir-timeout 60
  "Subprocess output timeout in seconds."
  :group 'org-babel
  :type 'integer)

(defcustom org-babel-elixir-table-flag nil
  "Non-nil means reassemble tables in the RESULTS."
  :group 'org-babel
  :type 'boolean)

(defconst org-babel-header-args:elixir
  '((cookie . :any)
    (name .  :any)
    (remsh . :any)
    (sname . :any)
    (cwd   . :any)
    (S     . :any))
  "Elixir header arguments.")

(defvar org-babel-elixir-filter-regexps
  '("\n\\(\\(iex\\|[.]+\\)\\(([^@]+@[^)]+)[0-9]+\\|([0-9]+)\\)> \\)+"
    "\\`[ \t\n]*"
    "[ \t\n]*\\'"
    "\r"
    "\1^M"
    "^import_file([^)]+)\n")
  "List of filter regex expressions.")

(defvar org-babel-elixir-raw-output ""
  "Auxiliary variable to hold process output.")

(defvar org-babel-elixir-eoe-indicator "\u2029"
  "End of evaluation indicator.")

;; (defvar org-babel-elixir-hline "\n")

(defmacro org-babel-elixir--message (fmt &rest args)
  "Display a internal message at the bottom of the screen.
See `message' for more information about FMT and ARGS arguments."
  `(message (concat "[ob-elixir]: ",fmt) ,@args))

(defun org-babel-elixir-process-output ()
  "Return process output."
  ;; update eoe-indicator
  (let ((eoe-indicator (format "\"%s\"" org-babel-elixir-eoe-indicator)))
    ;; parse output (remove eoe-indicator)
    (replace-regexp-in-string (regexp-quote eoe-indicator)
                              ""
                              org-babel-elixir-raw-output)))

(defun org-babel-elixir-process-wait ()
  "Wait for the the process output."
  (while (not (string-match-p
               org-babel-elixir-eoe-indicator
               org-babel-elixir-raw-output))
    (sit-for 0.1)))

(defun org-babel-elixir-send-string (process string)
  "Send STRING to Elixir PROCESS."
  (let ((string (format "%s\n" string)))
    ;; clean process raw output
    (setq org-babel-elixir-raw-output "")
    ;; send string to process
    (process-send-string process string)
    ;; accept process output (default timeout 1 minute)
    (accept-process-output process org-babel-elixir-timeout nil t)
    ;; send end indicator
    (process-send-string process
                         (format "\"%s\"\n"
                                 org-babel-elixir-eoe-indicator))
    ;; wait for the process
    (org-babel-elixir-process-wait)
    ;; return process raw output
    (org-babel-elixir-process-output)))

(defun org-babel-elixir-insert-string (process string)
  "Insert the STRING in the PROCESS buffer (debugging)."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          ;; insert the text, advancing the process marker.
          (goto-char (process-mark process))
          (insert string)
          (set-marker (process-mark process) (point)))
        (if moving (goto-char (process-mark process)))))))

(defun org-babel-elixir-process-filter (process output)
  "Filter PROCESS OUTPUT."
  ;; debug message
  (unless (process-live-p process)
    (org-babel-elixir--message "process die"))
  ;; insertion filer (test)
  (org-babel-elixir-insert-string process output)
  ;; concat raw process output
  (setq org-babel-elixir-raw-output
        (concat org-babel-elixir-raw-output output)))

(defun org-babel-elixir-start-process (name params)
  "Parse PARAMS to process args and start the process using its BUFFER-NAME."
  (let* ((buffer (get-buffer-create name))
         (program-args nil)
         (process nil))
    ;; with current buffer
    (with-current-buffer buffer
      ;; make a local environment variables for subprocesses list
      (make-local-variable 'process-environment)
      ;; set process environments list
      (setq process-environment (cons "TERM=vt100" process-environment)))
    ;; set program args
    (setq program-args (apply 'append
                              (org-babel-elixir-parse-process-params params)))
    ;; start the process
    (apply 'start-process name buffer org-babel-elixir-program program-args)
    ;; update process auxiliary variable
    (setq process (get-buffer-process name))
    ;; if process was properly created
    (when process
      ;; set process filter
      (set-process-filter process 'org-babel-elixir-process-filter)
      ;; send setup input (disable colors)
      (org-babel-elixir-send-string process
                                    "IEx.configure(colors: [enabled: false])"))
    ;; return process
    process))

(defun org-babel-elixir-parse-process-params (params)
  "Prepare process param options list."
  (let ((params-alist '((:sname "--sname")
                        (:name "--name")
                        (:cookie "--cookie")
                        (:remsh "--remsh")
                        (:S "-S")))
        ;; auxiliary variables
        (key nil)
        (option nil))
    ;; return process arguments list
    (delq nil
          (mapcar (lambda (param)
                    ;; update key and option
                    (setq key (car param))
                    (setq option (cdr param))
                    ;; get command line options
                    (when (assoc key params)
                      `(,(car option) ,(assoc-default key params))))
                  params-alist))))

(defun org-babel-elixir-var-to-elixir (var)
  "Convert an elisp VAR into a string of Elixir source code.
Specifying a VAR of the same value."
  (let ((values (when (or (listp var)
                          (vectorp var))
                  (mapconcat #'prin1-to-string var ", "))))
    (cond
     ;; if it's cons cell map it to tuples
     ((listp var) (concat "{" values "}"))
     ;; if it's a vector map it to elixir list
     ((vectorp var) (concat "[" values "]"))
     ;; default parse to string
     (t
      (if (eq var 'hline) nil
        (prin1-to-string var))))))

(defun org-babel-elixir--trim-string (results)
  "Remove white spaces in beginning and ending of RESULTS.
White space here is any of: space, tab, Emacs newline
(line feed, ASCII 10)."
  ;; set regex expression list
  (let ((regexps '("[ \t\n]*\\'"
                   "\\`[ \t\n]*"))
        ;; auxiliary string
        (string results))
    (dolist (regexp regexps)
      ;; update string
      (setq string (replace-regexp-in-string regexp "" string)))
    ;; return it
    string))

(defun org-babel-elixir-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  ;; safely convert tables into elisp lists
  (org-babel-script-escape
   (org-babel-elixir--trim-string results)))

(defun org-babel-elixir-insert-results (results table-flag)
  "Insert RESULTS and maybe parse its layout."
  ;; if parse if non-nil just return the 'raw' results
  (if (not table-flag) results
    (org-babel-elixir-table-or-string results)))

(defun org-babel-elixir-initiate-session (&optional session params)
  "If there is not a current inferior-process-buffer in SESSION
then create. Return the initialized session."
  (unless (string= session "none")
    (let* ((session (or session org-babel-elixir-program))
           (buffer (format "*%s*" session)))
      ;; return session comint buffer was already created
      (if (org-babel-comint-buffer-livep buffer)
          session
        ;; verify if elixir REPL program exists
        (when (executable-find org-babel-elixir-program)
          ;; set comint buffer
          (setq buffer
                (apply 'make-comint
                       session
                       org-babel-elixir-program
                       nil
                       (if (not params) nil
                         (org-babel-elixir-parse-process-params params))))
          ;; return session if (comint) buffer was created
          (if buffer session (error "make-comint (funcall) fail")))))))

(defun org-babel-elixir-evaluate-external-process (body params)
  ;; &optional result-type result-params column-names-p row-names-p)
  "Evaluate BODY in the Elixir external process."
  (let* ((name "*elixir-session-none*")
         ;; get default process
         (process (get-buffer-process name)))
    ;; restart iex process of necessary
    (unless (process-live-p process)
      ;; set process variable
      (setq process (org-babel-elixir-start-process name params)))
    ;; send string
    (let ((output (org-babel-elixir-send-string process body)))
      ;; if output size its not greater then 0, return nil,
      ;; otherwise the output
      (if (not (> (length output) 0)) nil output))))

(defun org-babel-elixir-evaluate-session (session body)
  "Evaluate BODY in SESSION (comint buffer)."
  (let* ((buffer (format "*%s*" session))
         (send-wait (lambda ()
                      (comint-send-input t nil)
                      (sleep-for 0 5)))
         (insert-line (lambda (line)
                        (insert line "\n")
                        (funcall send-wait)))
         (lines `("IEx.configure(colors: [enabled: false])"
                  ,body
                  "IEx.configure(colors: [enabled: true])"))
         ;; auxiliary
         (results '()))
    ;; set results (comint output)
    (setq results (org-babel-comint-with-output (buffer ":ok" t body)
                    (dolist (line lines)
                      (funcall insert-line line))))
    ;; return results (convert to strings)
    (prin1-to-string (butlast (nthcdr 2 results) 3))))

(defun org-babel-elixir-evaluate (session body params)
  "Evaluate julia code in BODY."
  (if session
      ;; use comint session buffer
      (org-babel-elixir-evaluate-session session body)
    ;; no session use external process
    (org-babel-elixir-evaluate-external-process body params)))

(defun org-babel-variable-assignments:elixir (params)
  "Return a list of Elixir statements assigning the block's variables."
  (let ((vars (org-babel--get-vars params))
        (value nil))
    ;; delete nil values
    (delq nil
          ;; generate a list of values in the format: str = str
          (mapcar (lambda (var)
                    ;; get variable value
                    (setq value (org-babel-elixir-var-to-elixir (cdr var)))
                    ;; when value its not nil parse it
                    (if (not value) nil
                      (format "%s=%s" (car var) value)))
                  vars))))

(defun org-babel-expand-body:elixir (body params)
  "Expand BODY , return the expanded body."
  ;; variable assignments
  (let* ((vars (org-babel-variable-assignments:elixir params))
         (temp-file (org-babel-temp-file "elixir-"))
         (full-body (concat
                     ;; map parsed variables
                     (mapc (lambda (var) (insert var "\n")) vars)
                     ;; strip a trailing space or carriage return from STRING
                     (org-babel-chomp body))))
    ;; insert into temporary file
    (with-temp-file temp-file
      (insert full-body))
    ;; return full-body
    (format "import_file(\"%s\")" temp-file)))

;;;###autoload
(defun org-babel-execute:elixir (body params)
  "Execute a block of Elixir code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ( ;; (params (org-babel-process-params params))
         ;; set the session if the session variable is non-nil
         (session (org-babel-elixir-initiate-session (cdr (assoc :session params))
                                                     params))
         ;; auxiliary
         (results nil))
    ;; expand body and evaluate
    (setq results
          (org-babel-elixir-evaluate session
                                     (org-babel-expand-body:elixir body params)
                                     params))
    (if (not results) nil
      ;; show a debug message, necessary?
      ;; (org-babel-elixir--message "evaluation fail, no results")
      ;; otherwise filter the raw output and return it
      (let ((regexps org-babel-elixir-filter-regexps))
        (prin1 (length results))
        ;; apply string replace using the list of regexps
        (dolist (regexp regexps)
          ;; update the process output string
          (setq results (replace-regexp-in-string regexp "" results)))
        ;; finally: insert the results
        (org-babel-elixir-insert-results results t)))))

;; add elixir to org-babel language extensions
(add-to-list 'org-babel-tangle-lang-exts '("elixir" . "iex"))

(provide 'ob-elixir)

;;; ob-elixir.el ends here
