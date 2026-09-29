;;; ob-janet.el --- Org-Babel support for the Janet language  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 FoAM oü

;; Author: nik gaffney <nik@fo.am>
;; Keywords: languages, tools, literate programming, janet
;; Homepage: https://codeberg.org/zzkt/ob-janet
;; Version: 1.0.5
;; Package-Requires: ((emacs "26.1") (org "9.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute and/or modify
;; it under the terms of the European Union Public Licence (EUPL)
;; https://interoperable-europe.ec.europa.eu/collection/eupl/

;;; Commentary:

;; Org-Babel support for Janet (https://janet-lang.org/)
;; Based on ob-template.el and previous work from DEADB17
;;  - https://github.com/DEADB17/ob-janet
;;  - https://github.com/DEADB17/ob-racket
;;
;; Setup:
;;   (add-to-list 'org-babel-load-languages '(janet . t))

;;; Code:

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

;;; Customization

(defgroup ob-janet nil
  "Org Babel support for Janet."
  :group 'org-babel
  :prefix "ob-janet-")

(defcustom ob-janet-executable "janet"
  "Janet executable name or path."
  :type 'string
  :group 'ob-janet)

(defcustom ob-janet-path nil
  "JANET_PATH environment variable. When non-nil, sets the module search path."
  :type '(choice (const :tag "Use system default" nil) string)
  :group 'ob-janet)

(defcustom ob-janet-hline-to "nil"
  "Replacement for table hlines in Janet input."
  :type 'string
  :group 'ob-janet)

(defcustom ob-janet-nil-to 'hline
  "Replacement for Janet nil in returned tables."
  :type 'symbol
  :group 'ob-janet)

(defcustom ob-janet-output-wrapper "%s"
  "Template wrapping body for :results output."
  :type 'string
  :group 'ob-janet)

(defcustom ob-janet-value-wrapper
  "(import spork/test)\n(pp (test/suppress-stdout (do %s)))"
  "Template wrapping body for :results value."
  :type 'string
  :group 'ob-janet)

(defvar org-babel-default-header-args:janet
  '((:results . "output"))
  "Default header arguments for Janet source blocks.")

(add-to-list 'org-babel-tangle-lang-exts '("janet" . "janet"))


;;; Elisp -> Janet conversion

(defun ob-janet--to-janet (value)
  "Convert Elisp VALUE to Janet syntax."
  (cond
   ((eq value 'hline) ob-janet-hline-to)
   ((null value) "nil")
   ((eq value t) "true")
   ((numberp value) (number-to-string value))
   ((stringp value) (format "%S" value))
   ((symbolp value) (format "'%s" value))
   ;; cons
   ((consp value)
    (if (and (cdr value) (atom (cdr value)))
        (ob-janet--to-janet-tuple value)
      (concat "(tuple "
              (mapconcat #'ob-janet--to-janet value " ")
              ")")))
   ;; vector
   ((vectorp value)
    (concat "(array "
            (mapconcat #'ob-janet--to-janet
                       (append value nil) " ")
            ")"))
   ;; hash table
   ((hash-table-p value)
    (let ((pairs nil))
      (maphash (lambda (k v)
                 (push (ob-janet--to-janet-hash k v)
                       pairs))
               value)
      (concat "(table " (mapconcat #'identity pairs " ") ")")))
   ;; other
   (t (format "%S" value))))

(defun ob-janet--to-janet-tuple (value)
  "Convert Elisp VALUE to Janet tuple syntax."
  (format "(tuple %s %s)"
          (ob-janet--to-janet (car value))
          (ob-janet--to-janet (cdr value))))


(defun ob-janet--to-janet-hash (key value)
  "Convert Elisp KEY, VALUE pair to Janet syntax."
  (format "%s %s"
          (ob-janet--to-janet key)
          (ob-janet--to-janet value)))


(defun ob-janet--vars-to-defs (vars)
  "Convert alist VARS to Janet (def name value) expressions."
  (mapconcat (lambda (pair)
               (format "(def %s %s)" (car pair) (ob-janet--to-janet (cdr pair))))
             vars "\n"))


;;; Body expansion

(defun org-babel-expand-body:janet (body params &optional processed-params)
  "Expand BODY with PARAMS, or optional PROCESSED-PARAMS to avoid re-processing."
  (let ((processed (or processed-params (org-babel-process-params params))))
    (with-temp-buffer
      (when-let* ((prologue (alist-get :prologue params)))
        (insert prologue "\n"))
      (let ((vars (org-babel--get-vars processed)))
        (when vars (insert (ob-janet--vars-to-defs vars) "\n")))
      (insert body)
      (when-let* ((epilogue (alist-get :epilogue params)))
        (insert epilogue "\n"))
      (buffer-string))))


;;; Output parsing

(defun ob-janet--parse-result (result)
  "Parse Janet RESULT string, substituting nil for `ob-janet-nil-to'."
  (let ((parsed (org-babel-script-escape (string-trim result))))
    (if (listp parsed)
        (mapcar (lambda (el) (if (equal el 'nil) ob-janet-nil-to el)) parsed)
      parsed)))


;;; Sessions

(defun ob-janet--session-p (session)
  "Return non-nil if SESSION is a valid session name."
  (and session (not (string= session "none"))))

(defconst ob-janet--session-echo-marker "OBJNT:ECO;"
  "Prefix for values echoed by the REPL after each form.")

(defconst ob-janet--session-value-marker "OBJNT:VAL;"
  "Prefix for the value of the last statement.")

(defconst ob-janet--session-end-marker "OBJNT:END;"
  "Prefix for the sentinel that marks the end of evaluation.")


(defun ob-janet--parse-session-output (output)
  "Parse session OUTPUT, removing REPL prompts and echo-marked lines."
  (let ((clean (replace-regexp-in-string
                (format "^%s[^\n]*\\(?:\n\\|\\'\\)"
                        (regexp-quote ob-janet--session-echo-marker))
                "" output)))
    (setq clean (replace-regexp-in-string "^repl:[0-9]+:> " "" clean))
    (replace-regexp-in-string "\\`\n+" "" clean)))


(defun ob-janet--extract-session-value (output)
  "Extract the value of the last statement from session OUTPUT."
  (or (and (string-match (format "^%s\\([^\n]*\\)"
                                 (regexp-quote ob-janet--session-value-marker))
                         output)
           (match-string 1 output))
      ""))


(defun ob-janet--initiate-session (&optional session)
  "Ensure a Janet REPL SESSION exists. Return buffer name."
  (let ((name (if (ob-janet--session-p session)
                  (format "janet-%s" session) "janet"))
        (process-environment
         (append '("TERM=dumb")
                 (when ob-janet-path
                   (list (concat "JANET_PATH=" ob-janet-path)))
                 process-environment)))
    (unless (comint-check-proc (format "*%s*" name))
      (make-comint name ob-janet-executable nil "-n")
      (accept-process-output nil 1)
      (with-current-buffer (format "*%s*" name)
        (set (make-local-variable 'comint-prompt-regexp)
             "^repl:[0-9]+:> ")))
    (format "*%s*" name)))


(defun ob-janet--session-payload (code value)
  "Build the forms sent to Janet from CODE (with VALUE flag for :result).

Installs an echo-marker in `curenv', evaluates CODE, optionally reads back
the value of the last statement (i.e. :results value), then prints a unique
sentinel so the caller knows evaluation is complete.
Returns \(PAYLOAD . SENTINEL)."
  (let ((sentinel (format "%s%s" ob-janet--session-end-marker
                          (random most-positive-fixnum))))
    (cons
     (concat
      (format "(put (curenv) :pretty-format \"%s%%q\")\n"
              ob-janet--session-echo-marker)
      code "\n"
      (when value
        (format "(print (string/format \"%s%%q\" (get-in (curenv) ['_ :value])))\n"
                ob-janet--session-value-marker))
      (format "(print \"%s\")\n" sentinel))
     sentinel)))


(defun ob-janet--wait-for-sentinel (proc sentinel start)
  "Wait for SENTINEL in PROC buffer, starting from START.
Return the position of the sentinel, or nil on timeout."
  (let ((deadline (+ (float-time) 5)))
    (while (and (< (float-time) deadline)
                (not (save-excursion
                       (goto-char (point-max))
                       (re-search-backward (regexp-quote sentinel)
                                           start t))))
      (accept-process-output proc 0.1)))
  (save-excursion
    (goto-char start)
    (let ((pos (re-search-forward (regexp-quote sentinel) nil t)))
      (when pos (match-beginning 0)))))


(defun ob-janet--execute-to-session (code session &optional value)
  "Send CODE to SESSION and return the result.

When VALUE is non-nil, return the value of the last statement evaluated,
otherwise return everything written to stdout."
  (with-current-buffer (ob-janet--initiate-session session)
    (let* ((proc (get-buffer-process (current-buffer)))
           (start (point-max))
           (protocol (ob-janet--session-payload code value))
           (payload (car protocol))
           (sentinel (cdr protocol)))
      (comint-send-string proc (concat payload "\n"))
      ;; Return everything between start and the sentinel
      (let* ((end (or (ob-janet--wait-for-sentinel proc sentinel start)
                      (point-max)))
             (text (buffer-substring-no-properties start end)))
        (if value
            (ob-janet--extract-session-value text)
          (ob-janet--parse-session-output text))))))


(defun ob-janet--execute-to-file (expanded file)
  "Execute EXPANDED code and write output to FILE."
  (let ((result (ob-janet--execute-external
                 expanded ob-janet-executable)))
    (with-temp-file file (insert result))
    nil))

(defun ob-janet--execute-external (code cmd)
  "Run CODE via CMD."
  (let ((file (org-babel-temp-file "ob-janet-" ".janet"))
        (process-environment
         (if ob-janet-path
             (cons (concat "JANET_PATH=" ob-janet-path) process-environment)
           process-environment)))
    (with-temp-file file (insert code))
    (org-babel-eval
     (concat (shell-quote-argument cmd) " "
             (org-babel-process-file-name file)) "")))


(defun ob-janet--result (result result-params processed)
  "Assemble raw Janet RESULT into the final block result.

Applies RESULT-PARAMS via `org-babel-result-cond', parsing the value with
`ob-janet--parse-result' when needed, then (re)adds any column and row names
requested through PROCESSED header arguments."
  (org-babel-reassemble-table
   (org-babel-result-cond result-params result
                          (ob-janet--parse-result result))
   (org-babel-pick-name (cdr (assq :colname-names processed))
                        (cdr (assq :colnames processed)))
   (org-babel-pick-name (cdr (assq :rowname-names processed))
                        (cdr (assq :rownames processed)))))


(defun org-babel-execute:janet (body params)
  "Execute Janet code BODY with header arguments PARAMS."
  (let* ((processed     (org-babel-process-params params))
         (session       (cdr (assq :session processed)))
         (result-type   (cdr (assq :result-type processed)))
         (result-params (cdr (assq :result-params processed)))
         (cmd           (alist-get :cmd params ob-janet-executable))
         (file          (alist-get :file params))
         (expanded      (org-babel-expand-body:janet
                         body params processed)))
    ;; debug
    (if (or (assoc :debug params) (assoc :debug processed))
        (concat (if (org-babel--get-vars processed)
                    (concat (ob-janet--vars-to-defs
                             (org-babel--get-vars processed))
                            "\n")
                  "")
                (if (string= result-type "value")
                    (format ob-janet-value-wrapper body)
                  (format ob-janet-output-wrapper body)))
      ;; session or file?
      (cond
       ((ob-janet--session-p session)
        ;; raw expanded body (no wrapper), defs persist across blocks
        (ob-janet--result
         (ob-janet--execute-to-session expanded session
                                       (string= result-type "value"))
         result-params processed))
       (file
        (ob-janet--execute-to-file expanded file) nil)
       (t
        (ob-janet--result
         (ob-janet--execute-external
          (format (if (string= result-type "value")
                      ob-janet-value-wrapper
                    ob-janet-output-wrapper)
                  expanded)
          cmd)
         result-params processed))))))


;;; Org-babel session functions

(defun org-babel-prep-session:janet (session _params)
  "Prepare a Janet SESSION."
  (unless (ob-janet--session-p session)
    (error "Janet sessions require a :session name"))
  (ob-janet--initiate-session session))

(defun org-babel-janet-initiate-session (&optional session)
  "Initialize a Janet SESSION buffer."
  (ob-janet--initiate-session session))

(defun org-babel-janet-session-info (&optional session)
  "Return info for SESSION."
  (format "Janet REPL: %s" (or session "default")))


(provide 'ob-janet)
;;; ob-janet.el ends here
