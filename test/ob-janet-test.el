;;; ob-janet-test.el --- ERT tests for ob-janet  -*- lexical-binding: t; -*-

;; This file was generated from testing.org any changes will be overwritten.

(require 'ert)
(require 'ob-janet)
(require 'org)

(add-to-list 'load-path
             (file-name-directory
              (or load-file-name buffer-file-name default-directory)))

(setq org-confirm-babel-evaluate nil)
(add-to-list 'org-babel-load-languages '(janet . t))
(org-babel-do-load-languages 'org-babel-load-languages
                             org-babel-load-languages)

(defun ob-janet-test-execute (source)
  "Execute the first source block in SOURCE and return its result."
  (with-temp-buffer
    (org-mode)
    (insert source)
    (goto-char (point-min))
    (org-babel-next-src-block)
    (org-babel-execute-src-block)))

(defun ob-janet-test-hash (pairs)
  "Return a hash table mapping each CAR of PAIRS to its CDR."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (pair pairs table)
      (puthash (car pair) (cdr pair) table))))

(defun ob-janet-test-hash-check (result pairs)
  "Assert RESULT serializes each key/value substring in PAIRS."
  (should (string-prefix-p "(table " result))
  (should (string-suffix-p ")" result))
  (dolist (pair pairs)
    (should (string-match-p pair result))))

(defmacro ob-janet-test-case (source expected)
  "Assert that `ob-janet--to-janet' converts SOURCE to EXPECTED."
  `(should (equal (ob-janet--to-janet ,source) ,expected)))

(defconst ob-janet-test-begin-src "#+begin_src")
(defconst ob-janet-test-end-src "#+end_src")

(defun ob-janet-test-src (header body)
  "Wrap BODY in an Org src block with HEADER."
  (format "%s %s\n%s\n%s\n"
          ob-janet-test-begin-src header body ob-janet-test-end-src))

(ert-deftest ob-janet-test-to-janet-atoms ()
  "Atomic values should convert to Janet literals."
  (ob-janet-test-case nil "nil")
  (ob-janet-test-case t "true")
  (ob-janet-test-case 0 "0")
  (ob-janet-test-case -5 "-5")
  (ob-janet-test-case 42 "42")
  (ob-janet-test-case 3.14 "3.14")
  (ob-janet-test-case "" "\"\"")
  (ob-janet-test-case "hello" "\"hello\"")
  (ob-janet-test-case 'foo "'foo")
  (let ((result (ob-janet--to-janet "a\tb")))
    (should (string-prefix-p "\"" result))
    (should (string-suffix-p "\"" result))
    (should (string= (substring result 1 -1) "a\tb"))))

(ert-deftest ob-janet-test-to-janet-special ()
  "Hlines map to `ob-janet-hline-to' and other objects should fall back."
  (ob-janet-test-case 'hline "nil")
  (let ((ob-janet-hline-to "''hline"))
    (ob-janet-test-case 'hline "''hline"))
  (let ((result (ob-janet--to-janet (current-buffer))))
    (should (stringp result))
    (should-not (string-empty-p result))))

(ert-deftest ob-janet-test-to-janet-collections ()
  "Proper lists, dotted pairs and vectors should convert to tuples/arrays."
  (ob-janet-test-case '(1 . 2) "(tuple 1 2)")
  (ob-janet-test-case '("a" . "b") "(tuple \"a\" \"b\")")
  (ob-janet-test-case '(1 2 3) "(tuple 1 2 3)")
  (ob-janet-test-case '(1) "(tuple 1)")
  (ob-janet-test-case '(1 "a" [2 3]) "(tuple 1 \"a\" (array 2 3))")
  (ob-janet-test-case '(("a" 1) ("b" 2)) "(tuple (tuple \"a\" 1) (tuple \"b\" 2))")
  (ob-janet-test-case '(("Ele" 36)) "(tuple (tuple \"Ele\" 36))")
  (ob-janet-test-case [1 2 3] "(array 1 2 3)")
  (ob-janet-test-case [] "(array )"))

(ert-deftest ob-janet-test-to-janet-hash ()
  "Hash tables should convert to Janet tables."
  (ob-janet-test-case (ob-janet-test-hash '(("a" . nil))) "(table \"a\" nil)")
  (ob-janet-test-hash-check
   (ob-janet--to-janet (ob-janet-test-hash '(("a" . 1) ("b" . 2))))
   '("\"a\" 1" "\"b\" 2"))
  (ob-janet-test-hash-check
   (ob-janet--to-janet
    (ob-janet-test-hash '(("a" . nil) ("b" . t) ("c" . 1.5) ("d" . sym) ("e" . "s"))))
   '("\"a\" nil" "\"b\" true" "\"c\" 1.5" "\"d\" 'sym" "\"e\" \"s\""))
  (ob-janet-test-hash-check
   (ob-janet--to-janet
    (ob-janet-test-hash `(("list" . (1 2))
                          ("vec" . [3 4])
                          ("table" . ,(ob-janet-test-hash '(("nested" . 1)))))))
   '("\"list\" (tuple 1 2)" "\"vec\" (array 3 4)" "\"table\" (table \"nested\" 1)"))
  (ob-janet-test-hash-check
   (ob-janet--to-janet (ob-janet-test-hash '((1 . "one") (:two . "TWO"))))
   '("1 \"one\"" "':two \"TWO\"")))

(ert-deftest ob-janet-test-to-janet-long-list ()
  "A proper list should maintain element order and type."
  (ob-janet-test-case '(1 2 3 4 5) "(tuple 1 2 3 4 5)")
  (ob-janet-test-case '(1 "two" 3.5 t nil foo)
                      "(tuple 1 \"two\" 3.5 true nil 'foo)"))

(ert-deftest ob-janet-test-to-janet-nesting ()
  "Nested lists should be converted recursively."
  (ob-janet-test-case '((1 2) (3 4) (5 6))
                      "(tuple (tuple 1 2) (tuple 3 4) (tuple 5 6))")
  (ob-janet-test-case '(1 (2 (3 (4 5))))
                      "(tuple 1 (tuple 2 (tuple 3 (tuple 4 5))))"))

(ert-deftest ob-janet-test-to-janet-cons-nesting ()
  "Dotted pairs and cons-headed pairs can nest inside lists."
  (ob-janet-test-case '(1 (2 . 3) 4) "(tuple 1 (tuple 2 3) 4)")
  (ob-janet-test-case '((1 . 2) (3 . 4)) "(tuple (tuple 1 2) (tuple 3 4))")
  (ob-janet-test-case '((1 2) . 3) "(tuple (tuple 1 2) 3)")
  (ob-janet-test-case '(1 . (2 3)) "(tuple 1 2 3)"))

(ert-deftest ob-janet-test-to-janet-empty-collections ()
  "Empty collections should be treated as scalar or array."
  (ob-janet-test-case '() "nil")
  (ob-janet-test-case [] "(array )")
  (ob-janet-test-case (make-hash-table) "(table )"))

(ert-deftest ob-janet-test-to-janet-mixed-collections ()
  "A list can hold vectors, hash tables, and scalars."
  (ob-janet-test-case
   (list 1 [2 3] (ob-janet-test-hash '(("k" . 1))) '(4 5))
   "(tuple 1 (array 2 3) (table \"k\" 1) (tuple 4 5))"))

(ert-deftest ob-janet-test-vars-to-defs ()
  "`ob-janet--vars-to-defs' should emit one def per variable."
  (should (equal (ob-janet--vars-to-defs '(("x" . 10) ("s" . "hi") ("f" . t)))
                 "(def x 10)\n(def s \"hi\")\n(def f true)"))
  (should (equal (ob-janet--vars-to-defs '(("xs" . [1 2])))
                 "(def xs (array 1 2))"))
  (should (equal (ob-janet--vars-to-defs nil) "")))

(ert-deftest ob-janet-test-parse-result ()
  "Numbers, strings and scalars round-trip the parser."
  (should (equal (ob-janet--parse-result "42") 42))
  (should (equal (ob-janet--parse-result "3.14") 3.14))
  (should (equal (ob-janet--parse-result "\"hello\"") "hello"))
  (should (equal (ob-janet--parse-result "true") "true"))
  (should (equal (ob-janet--parse-result "nil") "nil"))
  (should (equal (ob-janet--parse-result "@[1 2 3]") "@[1 2 3]")))

(ert-deftest ob-janet-test-parse-result-nil-to-hline ()
  "Top-level nil list elements become `ob-janet-nil-to'."
  (should (equal (ob-janet--parse-result "(1 nil 4)") '(1 hline 4)))
  (let ((ob-janet-nil-to "REPLACED"))
    (should (equal (ob-janet--parse-result "(1 nil)") '(1 "REPLACED")))))

(ert-deftest ob-janet-test-parse-result-shallow-nil ()
  "The nil substitution is shallow, nested nil is left alone."
  (should (equal (ob-janet--parse-result "((1 nil) (2 3))")
                 '((1 nil) (2 3)))))

(ert-deftest ob-janet-test-parse-session-output ()
  "REPL prompts and echo-marked lines are stripped."
  (should (equal (ob-janet--parse-session-output "repl:1:> (+ 1 1)\nrepl:2:> 2\n")
                 "(+ 1 1)\n2\n"))
  (should (equal (ob-janet--parse-session-output "\nrepl:10:> 42\n")
                 "42\n"))
  (should (equal (ob-janet--parse-session-output "repl:1:> x\n") "x\n"))
  (should (equal (ob-janet--parse-session-output
                  "OBJNT:ECO;@{}ntext\nOBJNT:ECO;nil\nhello\n")
                 "hello\n"))
  (should (equal (ob-janet--parse-session-output
                  "OBJNT:ECO;@{}\nsome text\nOBJNT:ECO;7\n")
                 "some text\n")))

(ert-deftest ob-janet-test-expand-body-vars ()
  "Variable definitions precede the body."
  (should (equal (org-babel-expand-body:janet "(+ x 1)" '((:var . "x=10")))
                 "(def x 10)\n(+ x 1)")))

(ert-deftest ob-janet-test-expand-body-plain ()
  "Without vars the body is passed through."
  (should (equal (org-babel-expand-body:janet "(print 1)" '()) "(print 1)"))
  (should (equal (org-babel-expand-body:janet "" '()) "")))

(ert-deftest ob-janet-test-expand-body-prologue-epilogue ()
  "Prologue and epilogue wrap the body."
  (should (equal (org-babel-expand-body:janet
                  "(print 1)"
                  '((:prologue . "(def p 1)") (:epilogue . "(print p)")))
                 "(def p 1)\n(print 1)(print p)\n"))
  (should (equal (org-babel-expand-body:janet
                  "(+ x 1)"
                  '((:prologue . "(def a 1)")
                    (:var . "x=10")
                    (:epilogue . "(print a)")))
                 "(def a 1)\n(def x 10)\n(+ x 1)(print a)\n")))

(ert-deftest ob-janet-test-execute-value ()
  "`:results value' returns the block's return value."
  (skip-unless (executable-find ob-janet-executable))
  (should (equal (org-babel-execute:janet "(+ 1 2)" '((:results . "value"))) 3))
  (should (equal (org-babel-execute:janet "(+ 1.5 2.25)" '((:results . "value"))) 3.75))
  (should (equal (org-babel-execute:janet "0xff" '((:results . "value"))) 255)))

(ert-deftest ob-janet-test-execute-output ()
  "`:results output' captures stdout."
  (skip-unless (executable-find ob-janet-executable))
  (should (equal (org-babel-execute:janet "(print (+ 1 2))" '((:results . "output")))
                 "3\n"))
  (should (equal (org-babel-execute:janet "(print \"a\")\n(print \"b\")" '((:results . "output")))
                 "a\nb\n")))

(ert-deftest ob-janet-test-execute-raw ()
  "`:results output raw' returns the unparsed output."
  (skip-unless (executable-find ob-janet-executable))
  (should (equal (org-babel-execute:janet "(print (+ 1 2))" '((:results . "output raw")))
                 "3\n")))

(ert-deftest ob-janet-test-execute-none ()
  "`:results none' runs the block but inserts no result."
  (skip-unless (executable-find ob-janet-executable))
  (with-temp-buffer
    (org-mode)
    (insert "\n" (ob-janet-test-src "janet :results none" "(print \"hi\")"))
    (goto-char (point-min))
    (org-babel-next-src-block)
    (should (org-babel-execute-src-block))
    (should-not (org-babel-where-is-src-block-result))
    (should-not (re-search-forward "#\\+RESULTS:" nil t))))

(ert-deftest ob-janet-test-execute-error ()
  "A Janet error writes to stderr; stdout stays empty."
  (skip-unless (executable-find ob-janet-executable))
  (should (equal (org-babel-execute:janet "(error \"boom\")" '((:results . "output")))
                 "")))

(ert-deftest ob-janet-test-execute-value-types ()
  "Janet values round-trip through `:results value'."
  (skip-unless (executable-find ob-janet-executable))
  (should (equal (org-babel-execute:janet "(tuple 1 2 3)" '((:results . "value")))
                 '(1 2 3)))
  (should (equal (org-babel-execute:janet "@[1 2 3]" '((:results . "value")))
                 "@[1 2 3]"))
  (should (equal (org-babel-execute:janet "nil" '((:results . "value")))
                 "nil"))
  (should (equal (org-babel-execute:janet "true" '((:results . "value")))
                 "true"))
  (should (equal (org-babel-execute:janet "(quote foo)" '((:results . "value")))
                 "foo")))

(ert-deftest ob-janet-test-execute-var-scalar ()
  "Scalar `:var' values are injected as defs."
  (skip-unless (executable-find ob-janet-executable))
  (should (equal (org-babel-execute:janet "(+ x 1)" '((:results . "value") (:var . "x=41")))
                 42))
  (should (equal (org-babel-execute:janet "x" '((:results . "value") (:var . "x=\"hi\"")))
                 "hi")))

(ert-deftest ob-janet-test-execute-var-collection ()
  "Collection `:var' values convert to Janet collections."
  (skip-unless (executable-find ob-janet-executable))
  (should (equal (org-babel-execute:janet "xs" '((:results . "value") (:var . "xs=[1 2 3]")))
                 "@[1 2 3]"))
  (should (equal (org-babel-execute:janet "p" '((:results . "value") (:var . "p='(1 2 3)")))
                 '(1 2 3))))

(ert-deftest ob-janet-test-execute-table-multirow ()
  "Multi-row tables pass as a tuple of row tuples."
  (skip-unless (executable-find ob-janet-executable))
  (should (equal (ob-janet-test-execute
                  (concat "#+name: chars\n"
                          "| Name | Age |\n"
                          "|------+-----|\n"
                          "| Ele | 36 |\n"
                          "| Sam | 41 |\n"
                          "\n"
                          (ob-janet-test-src
                           "janet :var data=chars :results output"
                           "(each row data (print (string (row 0) \":\" (row 1))))")))
                 "Ele:36\nSam:41\n")))

(ert-deftest ob-janet-test-execute-table-single-row ()
  "A single-row table must not gain a spurious nil element."
  (skip-unless (executable-find ob-janet-executable))
  (should (equal (ob-janet-test-execute
                  (concat "#+name: one\n"
                          "| Name |\n"
                          "|------|\n"
                          "| Sol  |\n"
                          "\n"
                          (ob-janet-test-src
                           "janet :var data=one :results output"
                           "(each row data (print (row 0)))")))
                 "Sol\n")))

(ert-deftest ob-janet-test-execute-debug ()
  "`:debug' prints the expanded body without evaluation."
  (skip-unless (executable-find ob-janet-executable))
  (should (equal (org-babel-execute:janet
                  "(+ x 1)" '((:results . "value") (:var . "x=41") (:debug)))
                 "(def x 41)\n(import spork/test)\n(pp (test/suppress-stdout (do (+ x 1))))"))
  (should (equal (org-babel-execute:janet
                  "(+ x 1)" '((:results . "output") (:var . "x=41") (:debug)))
                 "(def x 41)\n(+ x 1)")))

(ert-deftest ob-janet-test-execute-file ()
  "`:file' writes the output to a file and returns nil."
  (skip-unless (executable-find ob-janet-executable))
  (let ((file (org-babel-temp-file "ob-janet-test-" ".txt")))
    (unwind-protect
        (progn
          (should-not (org-babel-execute:janet
                       "(print 42)" `((:results . "output") (:file . ,file))))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "42\n")))
      (when (file-exists-p file) (delete-file file)))))

(defun ob-janet-test-cleanup-session (name)
  "Kill the Janet REPL process and buffer for session NAME."
  (let* ((buffer-name (format "*janet-%s*" name))
         (process (get-buffer-process buffer-name)))
    (when process (delete-process process))
    (when (get-buffer buffer-name) (kill-buffer buffer-name))))

(ert-deftest ob-janet-test-session-p ()
  "A session is valid unless nil or \"none\"."
  (should-not (ob-janet--session-p nil))
  (should-not (ob-janet--session-p "none"))
  (should (ob-janet--session-p "foo")))

(ert-deftest ob-janet-test-session-info ()
  "`org-babel-janet-session-info' names the session."
  (should (string= (org-babel-janet-session-info) "Janet REPL: default"))
  (should (string= (org-babel-janet-session-info "foo") "Janet REPL: foo")))

(ert-deftest ob-janet-test-prep-session-requires-name ()
  "`org-babel-prep-session:janet' rejects unnamed sessions."
  (should-error (org-babel-prep-session:janet "none" nil))
  (should-error (org-babel-prep-session:janet nil nil)))

(ert-deftest ob-janet-test-prep-session ()
  "`org-babel-prep-session:janet' starts a named REPL buffer."
  (skip-unless (executable-find ob-janet-executable))
  (unwind-protect
      (let ((buffer (org-babel-prep-session:janet "ert-sess" nil)))
        (should (string= buffer "*janet-ert-sess*"))
        (should (buffer-live-p (get-buffer buffer))))
    (ob-janet-test-cleanup-session "ert-sess")))

(ert-deftest ob-janet-test-initiate-session ()
  "`ob-janet--initiate-session' returns a live REPL buffer."
  (skip-unless (executable-find ob-janet-executable))
  (unwind-protect
      (let ((buffer (ob-janet--initiate-session "ert-init")))
        (should (string= buffer "*janet-ert-init*"))
        (should (get-buffer-process buffer)))
    (ob-janet-test-cleanup-session "ert-init")))

(ert-deftest ob-janet-test-execute-session ()
  "Named blocks share state, output mode shows only stdout."
  (skip-unless (executable-find ob-janet-executable))
  (unwind-protect
      (progn
        (should (equal (org-babel-execute:janet
                        "(def n 7)" '((:session . "ert-exec") (:results . "output")))
                       ""))
        (should (equal (org-babel-execute:janet
                        "(print (* n 2))" '((:session . "ert-exec") (:results . "output")))
                       "14\n")))
    (ob-janet-test-cleanup-session "ert-exec")))

(ert-deftest ob-janet-test-execute-session-output-final-print ()
  "Output mode captures print output, including a final print."
  (skip-unless (executable-find ob-janet-executable))
  (unwind-protect
      (progn
        (should (equal (org-babel-execute:janet
                        "(print \"one\")\n(print \"two\")\n(print \"three\")"
                        '((:session . "ert-out") (:results . "output")))
                       "one\ntwo\nthree\n"))
        (should (equal (org-babel-execute:janet
                        "(defn say [x] (print x))\n(say \"final\")"
                        '((:session . "ert-out") (:results . "output")))
                       "final\n")))
    (ob-janet-test-cleanup-session "ert-out")))

(ert-deftest ob-janet-test-execute-session-value ()
  "Value mode returns the last statement of a session."
  (skip-unless (executable-find ob-janet-executable))
  (unwind-protect
      (progn
        (should (equal (org-babel-execute:janet
                        "(def n 7)" '((:session . "ert-val") (:results . "value")))
                       7))
        (should (equal (org-babel-execute:janet
                        "(+ n 2)" '((:session . "ert-val") (:results . "value")))
                       9)))
    (ob-janet-test-cleanup-session "ert-val")))

(ert-deftest ob-janet-test-execute-session-value-non-scalar ()
  "Session value mode matches non-session value mode for collections."
  (skip-unless (executable-find ob-janet-executable))
  (unwind-protect
      (progn
        (should (equal (org-babel-execute:janet
                        "@[1 2 3]" '((:session . "ert-val2") (:results . "value")))
                       "@[1 2 3]")))
    (ob-janet-test-cleanup-session "ert-val2")))

(provide 'ob-janet-test)

(when noninteractive
  (ert-run-tests-batch-and-exit "ob-janet"))

;;; ob-janet-test.el ends here
