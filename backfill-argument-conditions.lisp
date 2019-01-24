;;; The programmer should tell us what type they expect to pass to
;;; functions returned by a polymorphic call.
;;;
;;; When they don't, let's warn and come up with *something*.
(defpackage "BACKFILL-ARGUMENT-CONDITIONS"
  (:use "CL" "OCCUR-CHECK")
  (:export "BACKFILL")
  (:local-nicknames ("C" "CONDITION")
                    ("S" "SKELETON-TYPE")
                    ("FLOW-INFO" "GATHER-SKELETON-FLOW-INFO")
                    ("CONDITIONS" "GATHER-ARGUMENT-CONDITIONS")))

(in-package "BACKFILL-ARGUMENT-CONDITIONS")

(defvar *flow-info*)
(declaim (type flow-info:info *flow-info*))

(defvar *conditions*)
(declaim (type conditions:conditions *conditions*))

(defun should-backfill (base)
  (declare (type s:base base))
  (destructuring-bind (name polarity flow)
      (s:split base)
    (declare (ignore name flow))
    (and (eql polarity '-)
         (not (conditions:base-condition *conditions* base)))))

(defun generate-condition-for-base (base)
  (check-type base s:base)
  (with-occur-check ((base) (return 'c:true))
    (let* ((condition
            (or (conditions:base-condition *conditions* base)
                (progn
                  (assert (conditions:base-condition *conditions* base))
                  (return 'c:true))))
           (to-bind (ordered:map :test #'equalp))
           (rewritten
            (scoping:to-local
             condition #'s:base-p
             (lambda (expression in-scope-p)
               (assert (not in-scope-p))
               ;; make up a new binding that will then be made to look
               ;; like something bound to this s:base.
               (or (ordered:find to-bind expression)
                   (let ((fresh (c:fresh '#:h)))
                     (ordered:record to-bind (cons expression fresh))
                     fresh)))
             :test #'equalp)))
      (wrap-existentials to-bind rewritten))))

(defun wrap-existentials (to-bind condition)
  (check-type to-bind ordered:map)
  (let ((bindings '())
        (conditions (list condition)))
    (loop for (base . gensym) in (ordered:entries to-bind) do
         (let ((condition (generate-condition-for-base base)))
           (push `(,gensym  ,(flow-info:base-sort *flow-info* base)) bindings)
           (push (if (c:mentions-v condition)
                     `(let ((c:v ,gensym))
                        ,condition)
                     condition)
                 conditions)))
    (if bindings
        `(c:exists ,(nreverse bindings)
                   ,(c:and-conditions (nreverse conditions)))
        condition)))

(defun generate-backfill-condition (base)
  (check-type base s:base)
  (with-occur-check-environment (#'equalp)
    (with-occur-check ((base) (error "Occur check failed at root"))
      ;; heuristically try to satisfy an (over) approximation of the
      ;; intersection of all the conditions to which the callee may
      ;; push values bound to this binding.
      ;;
      ;; The sinks all come from the (monotyped) argument list, so
      ;; they only refer to sources in that list, which are known to
      ;; have a specification.
      ;;
      ;; We do not need, nor want, to take into account constraints
      ;; imposed by the polymorphic function type itself: allegedly,
      ;; the function's implementation inhabits its type.
      ;;
      ;; Constraints on values flowing in the function (negative
      ;; polarity) are redundant, otherwise the call will eventually
      ;; fail to typecheck. Constraints on values flowing out of the
      ;; function (positive polarity) have already been checked to
      ;; always be satisfied. For example, if a polymorphic function
      ;; guarantees that it will return a positive value, the
      ;; implementation has (should have) been checked to satisfy its
      ;; postcondition for any set of valid arguments.
      ;;
      ;; If this polymorphic value goes nowhere, we'll let it take any
      ;; value... but it should still have been assigned a sort, so
      ;; that's a strange situation.
      (c:and-conditions
       (mapcar #'generate-condition-for-base
               (flow-info:base-sinks *flow-info* base))))))

(defun backfill (skeleton *flow-info* *conditions*)
  (check-type skeleton s:function)
  (check-type *flow-info* flow-info:info)
  (check-type *conditions* conditions:conditions)
  (scoping:with-environment ()
    ;; nothing can refer to the function scope: we're only looking at
    ;; arguments, and they can't refer to each other by de bruijn
    ;; index.
    (dolist (base (flow-info:all-bases *flow-info*) *conditions*)
      (assert (not (member (flow-info:base-sort *flow-info* base)
                           '(nil *))))
      (when (should-backfill base)
        (let ((backfilled (generate-backfill-condition base)))
          (format *error-output*
                  "Hallucinated condition for ~A:~%    ~A.~%"
                  base backfilled)
          (conditions:adjoin-condition *conditions* base backfilled))))))
