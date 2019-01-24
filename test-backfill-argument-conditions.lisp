(defpackage "TEST-BACKFILL-ARGUMENT-CONDITIONS"
  (:use "CL" "PROVE")
  (:local-nicknames ("C" "CONDITION")
                    ("F" "FRESH")
                    ("M" "MONO-TYPE")
                    ("P" "POLY-TYPE")
                    ("S" "SKELETON-TYPE")))

(in-package "TEST-BACKFILL-ARGUMENT-CONDITIONS")

(setf prove:*default-test-function* #'equalp)

(plan 7)

;;; (a -> b) -> (b -> c) -> a -> c, with funny side constraints
;;; should inherit the condition for a from the first argument.

(defparameter *poly* (p:parse
                      `(p:function ((p:function ((p:base (foo) integer nil))
                                                ((p:function ((p:base (a) * nil))
                                                             ((p:box (p:base (b) * nil))))))
                                    (p:function ((p:base (b) * nil))
                                                ((p:base (c) * nil))))
                                   ;; this condition should not be gathered.
                                   ((p:function ((p:base (a) * '(>= c:v -10)))
                                                ((p:base (c) * nil)))))))

(defparameter *return* (p:parse `((p:function ((p:base (x) * nil))
                                              ((p:base (y) * nil))))))

;; with an explicit return, we should use the return type's condition.
(defparameter *explicit-return*
  (p:parse `((p:function ((p:base (x) * (>= c:v 2)))
                         ((p:base (y) * nil))))))

(defparameter *mono* (m:parse
                      `((m:function ((m:base integer (/= c:v 2)))
                                    ((m:function ((m:base integer (and (>= c:v 0)
                                                                       (/= c:v (c:@- 0 1)))))
                                                 ((m:box (m:base integer c:true))))))
                        (m:function ((m:base integer c:true))
                                    ((m:base integer c:true))))))

(let ((c:*counter* 0)
      skeleton sort-solution flow-info argument-conditions)
  (is (setf skeleton (poly-to-skeleton:convert *poly* *mono*))
      (s:parse `(s:function ((s:function ((s:base f:b0 + (foo)))
                                         ((s:function ((s:base f:b1 + (a)))
                                                      ((s:box (s:base f:b2 - (b)))))))
                             (s:function ((s:base f:b3 + (b)))
                                         ((s:base f:b4 - (c)))))
                            ((s:function ((s:base f:b5 - (a)))
                                         ((s:base f:b6 + (c))))))))
  (isnt (setf sort-solution
              (solve-sort-constraints:solution skeleton
                                               *poly*
                                               *mono*
                                               *return*))
        nil)
  (isnt (setf flow-info
              (gather-skeleton-flow-info:info skeleton sort-solution))
        nil)

  ;; First, check that we inherit the condition from an explicit return type.
  (is (gather-argument-conditions:base-condition
       (gather-argument-conditions:conditions *poly* skeleton
                                              *mono* *explicit-return*)
       (s:parse `(s:base f:b5 - (a))))
      `(>= c:v 2))

  ;; Next, test type hallucination.
  (isnt (setf argument-conditions
              (gather-argument-conditions:conditions *poly* skeleton
                                                     *mono* *return*))
        nil)

  ;; confirm the returned function's argument isn't constrained
  (is (gather-argument-conditions:base-condition argument-conditions
                                                 (s:parse `(s:base f:b5 - (a))))
      nil)

  ;; backfill should succeed and propagate the constraint to the argument
  (is (progn
        (backfill-argument-conditions:backfill
         skeleton flow-info argument-conditions)
        (gather-argument-conditions:base-condition
         argument-conditions
         (s:parse `(s:base f:b5 - (a)))))
      `(c:exists ((f:h7 integer))
                 (and (>= c:v 0)
                      (/= c:v f:h7)
                      (let ((c:v f:h7))
                        (/= c:v 2))))))

(finalize)
