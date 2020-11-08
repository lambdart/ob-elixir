;;; ob-elixir.el --- Org Babel functions for Elixir evaluation -*- lexical-binding: t; -*-
;;
;; Author: esac <esac-io@tutanota.com>
;; Homepage: https://github.com/esac-io/ob-elixir
;; Keywords: org-mode ob-elixir elixir iex
;; Version: 0.0.3 Alpha
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 esac
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
(require 'ob-tangle)

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
    (sname . :any))
  "Elixir header arguments.")

(defvar ob-babel-elixir-filter-regexps
  '("\n\\(\\(iex\\|[.]+\\)\\(([^@]+@[^)]+)[0-9]+\\|([0-9]+)\\)> \\)+"
    "\\`[ \t\n]*"
    "[ \t\n]*\\'"
    "\r"
    "^import_file([^)]+)\n")
  "List of filter regex expressions.")

(defvar ob-babel-elixir-raw-output ""
  "Auxiliary variable to hold process output.")

(defvar ob-babel-elixir-eoe-indicator "\u2029"
  "A string to indicate that evaluation has completed.")

(defvar ob-babel-elixir-hline nil
  "Default hline value.")

(defun ob-babel-elixir-proc-output ()
  "Return process output."
  ;; update eoe-indicator
  (let ((eoe-indicator (format "\"%s\"" ob-babel-elixir-eoe-indicator)))
    ;; parse output (remove eoe-indicator)
    (replace-regexp-in-string (regexp-quote eoe-indicator)
                              ""
                              ob-babel-elixir-raw-output)))

(defun ob-babel-elixir-proc-wait ()
  "Wait for the the process output."
  (while (not (string-match-p
               ob-babel-elixir-eoe-indicator
               ob-babel-elixir-raw-output))
    (sit-for 0.1)))

(defun ob-babel-elixir-send-string (process string)
  "Send STRING to Elixir PROCESS."
  (let ((string (format "%s\n" string)))
    ;; clean process raw output
    (setq ob-babel-elixir-raw-output "")
    ;; send string to process
    (process-send-string process string)
    ;; accept process output (default timeout 1 minute)
    (accept-process-output process org-babel-elixir-timeout nil t)
    ;; send end indicator
    (process-send-string process
                         (format "\"%s\"\n"
                                 ob-babel-elixir-eoe-indicator))
    ;; wait for the process
    (ob-babel-elixir-proc-wait)
    ;; return process raw output
    (ob-babel-elixir-proc-output)))

(defun ob-babel-elixir-evaluate (process body)
  "Evaluate BODY in the Elixir PROCESS session."
  (let ((output (ob-babel-elixir-send-string process body)))
    ;; if output size its not greater then 0, return nil
    (if (not (> (length output) 0)) nil
      ;; otherwise filter the raw output and return it
      (let ((regexps ob-babel-elixir-filter-regexps))
        ;; apply string replace using the list of regexps
        (dolist (regexp regexps)
          ;; update the process output string
          (setq output (replace-regexp-in-string regexp "" output))))
      ;; return output
      output)))

(defun ob-babel-elixir-proc-filter (process output)
  "Filter PROCESS OUTPUT."
  ;; debug message
  (unless (process-live-p process)
    (message "process die"))
  ;; concat raw process output
  (setq ob-babel-elixir-raw-output
        (concat ob-babel-elixir-raw-output output)))

(defun ob-babel-elixir-start-process (name params)
  "Parse PARAMS to process args and start the process using its BUFFER-NAME."
  (let* ((buffer (get-buffer-create name))
         (program-args nil)
         (process nil))
    ;; with current buffer
    (with-current-buffer buffer
      ;; make a local environment variables for subprocesses list
      (make-local-variable 'process-environment)
      ;; set process environments list
      (setq process-environment (cons "TERM=vt100" process-environment))
      ;; set program args
      (setq program-args (apply 'append
                                (org-babel-elixir-parse-proc-params params)))
      ;; start the process
      (apply 'start-process name buffer org-babel-elixir-program program-args))
    ;; update process auxiliary variable
    (setq process (get-buffer-process name))
    ;; if process was properly created
    (when process
      ;; set process filter
      (set-process-filter process 'ob-babel-elixir-proc-filter)
      ;; send setup input (disable colors)
      (ob-babel-elixir-send-string process
                                   "IEx.configure(colors: [enabled: false])"))
    ;; return process
    process))

(defun org-babel-elixir-parse-proc-params (params)
  "Prepare process param options list."
  (let ((params-alist '((:sname  "--sname")
                        (:name   "--name")
                        (:cookie "--cookie")
                        (:remsh  "--remsh")))
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

(defun org-babel-elixir-initiate-session (session params)
  "If there is not a current inferior-process-buffer in SESSION
then create. Return the initialized session."
  (let* ((name (format "*elixir-%s*" session)) ;; set process buffer name
         ;; get default process
         (process (get-buffer-process name)))
    ;; start (iex) process if isn't already alive
    (unless (process-live-p process)
      ;; set process variable
      (setq process (ob-babel-elixir-start-process name params)))
    ;; return process or nil (implicit)
    process))

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
    (with-temp-file temp-file (insert full-body))
    ;; return temp-file
    temp-file))

;;;###autoload
(defun org-babel-execute:elixir (body params)
  "Execute a block of Elixir code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((session (cdr (assoc :session params)))
         ;; get default session process
         (process (org-babel-elixir-initiate-session session params))
         ;; auxiliary
         (results nil))
    ;; verify process
    (if (not process)
        (error "[ob-elixir]: missing inferior process")
      ;; expand body and evaluate
      (setq results
            (ob-babel-elixir-evaluate process
                                      (format "import_file(\"%s\")"
                                              (org-babel-expand-body:elixir body params))))
      ;; verify evaluation results
      (if (not (eq results nil))
          ;; finally: insert the results
          (org-babel-elixir-insert-results results
                                           org-babel-elixir-table-flag)
        ;; debug message
        (message "[ob-elixir]: evaluation fail, no results")
        ;; return nil
        nil))))

;; add elixir to org-babel language extensions
(add-to-list 'org-babel-tangle-lang-exts '("elixir" . "iex"))

(provide 'ob-elixir)

;;; ob-elixir.el ends here
