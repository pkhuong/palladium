(defpackage "CONDITION"
  (:use "CL")
  ;; XXX: the * here should be the canonical "no sort defined" symbol.
  (:export "*" "*COUNTER*" "TRUE" "FALSE" "EXISTS"
           "@+" "@-" "V" "_" "FRESH"
           "OR-CONDITIONS" "AND-CONDITIONS" "MENTIONS-V"
           "RENAME-VARS-FOR-SMTLIB"))

(defpackage "FRESH"
  (:use)
  (:export . #.(loop for i below 100 append
                    (loop for prefix in '(#:v #:s #:t #:b #:h #:a #:e #:k)
                       collect (make-symbol (format nil "~A~A" prefix i))))))

(in-package "CONDITION")

(defconstant true 'true)
(defconstant false 'false)
(defconstant exists 'exists)
(defconstant @+ '@+
  "Symbol to refer to function results")
(defconstant @- '@-
  "Symbol to refer to function arguments")
(defconstant _ '_)

(defvar *counter*)
(declaim (type (and unsigned-byte fixnum) *counter*))

;; s: used to substitute spread variables away in pattern-to-poly.
;; t: used to generate new base flows in poly-to-skeleton.
;; b: used to generate unique names in skeletons.
;; h: used to generate hallucinated premises in backfill-argument-conditions.
;; a: for approximations in synthesise-local-conditions.
(defun fresh (&optional (prefix '#:v))
  (let* ((counter (prog1 *counter*
                    (incf *counter*)))
         (name (intern (format nil "~A~A" prefix counter)
                       (load-time-value (find-package "FRESH")))))
    ;; the plist is initally empty, and no one should be using our
    ;; internal package.
    (unless (symbol-plist name)
      ;; Populate the symbol's plist with a preparsed version of the identifier.
      ;; We set the prefix to ^foo because cl-smt will then print that as just 'foo',
      ;; without any package prefix.
      (setf (get name :prefix) (intern
                                (concatenate 'string "^" (symbol-name prefix))
                                (load-time-value (find-package "FRESH")))
            (get name :counter) counter))
    name))

(defun mentions-v (condition)
  (etypecase condition
    ((eql v) t)
    ;; XXX: detect shadowing bindings.
    (list
     (some #'mentions-v condition))
    (t
     nil)))

(defun and-conditions (conditions)
  (let ((uniques (ordered:set :test #'equalp)))
    (labels ((walk-conditions (conditions)
               (dolist (condition conditions)
                 (etypecase condition
                   ((eql true)) ;; nothing to do
                   ((eql false)
                    (return-from and-conditions 'false))
                   ((cons (eql and))
                    (walk-conditions (rest condition)))
                   (t
                    (ordered:record uniques condition))))))
      (walk-conditions conditions))
    (let ((uniques (ordered:entries uniques)))
      (cond ((null uniques)
             'true)
            ((null (rest uniques))
             (first uniques))
            (t
             `(and ,@uniques))))))

(defun or-conditions (conditions)
  (let ((uniques (ordered:set :test #'equalp)))
    (labels ((walk-conditions (conditions)
               (dolist (condition conditions)
                 (etypecase condition
                   ((eql false))
                   ((eql true)
                    (return-from or-conditions 'true))
                   ((cons (eql or))
                    (walk-conditions (rest condition)))
                   (t
                    (ordered:record uniques condition))))))
      (walk-conditions conditions))
    (let ((uniques (ordered:entries uniques)))
      (cond ((null uniques)
             'false)
            ((null (rest uniques))
             (first uniques))
            (t
             `(or ,@uniques))))))

(defun rename-vars-for-smtlib (expression)
  (etypecase expression
    (list (mapcar #'rename-vars-for-smtlib expression))
    (symbol (if (eql (symbol-package expression)
                     (load-time-value (find-package "FRESH")))
                ;; (_ key index) is the SMT lib syntax for indexed identifiers
                `(_ ,(get expression :prefix) ,(get expression :counter))
                expression))
    (t expression)))
