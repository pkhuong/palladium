(defpackage "TEST-ELABORATE-MONOMORPHIC-TYPE"
  (:use "CL" "PROVE")
  (:import-from "CONDITION" "V" "EXISTS" "@-" "@+" "TRUE")
  (:local-nicknames ("C" "CONDITION")
                    ("F" "FRESH")
                    ("M" "MONO-TYPE")
                    ("P" "PATTERN-TYPE")
                    ("MONOMORPHISE" "ELABORATE-MONOMORPHIC-TYPE")))

(in-package "TEST-ELABORATE-MONOMORPHIC-TYPE")

(setf prove:*default-test-function* #'equalp)

(plan 3)

;;; Driving example: derive dataflow for the composition (.) operator.
(is (monomorphise:elaborate
     (p:parse `(p:function ((p:function ((p:spread (a)))
                                        ((p:spread (b))))
                            (p:function ((p:spread (b)))
                                        ((p:spread (c)))))
                           ((p:function ((p:spread (a)))
                                        ((p:spread (c)))))))
     (m:parse `((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (- (@- 0) 1)))))
                (m:function ((m:base integer true))
                            ((m:base integer (= v (+ (@- 0) 1)))))))
     (p:parse `((p:spread (a)))))
    ;; expectation:
    ;;    ({int a : v >= 0} -> {int : v = a - 1})
    ;; -> ({int a : v = x - 1, x >= 0} -> {int : v = a + 1}
    ;; -> ({int a : v >= 0} -> {int : v = x + 1, x = a - 1})
    (m:parse `(m:function
               ((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (- (@- 0) 1)))))
                (m:function ((m:base integer (exists
                                              ((f:a13 integer))
                                              (and (= v (- f:a13 1))
                                                   (let ((v f:a13))
                                                     (>= v 0))))))
                            ((m:base integer (= v (+ (@- 0) 1))))))
               ((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (exists
                                              ((f:a14 integer))
                                              (and (= v (+ f:a14 1))
                                                   (let ((v f:a14))
                                                     (exists
                                                      ((f:a15 integer))
                                                      (and (= v (- f:a15 1))
                                                           (let ((v f:a15))
                                                             (= v (@- 0)))))))))))))))

;;; Check that we preserve relational postconditions and correctly
;;; parse relations in the expected result type.
(is (monomorphise:elaborate
     (p:parse `(p:function ()
                           ((p:base (x) integer (> v 0))
                            (p:function ((p:base (y) integer nil))
                                        ()))))
     '()
     (p:parse `((p:base (x) integer true)
                (p:function ((p:base (y) integer (>= v (@- 0 1))))
                            ((p:spread (z)))))))
    (m:parse `(m:function ()
                          ((m:base integer (> v 0))
                           (m:function ((m:base integer (>= v (@+ 0 1))))
                                       ())))))

;;; Make sure we don't croak on fold.
;;;
;;; This example shows three things:
;;;
;;;  1. we can derive something useful for loop invariants
;;;  2. we will detect type mismatches when doing so.
;;;  3. we will also correctly intersect in the polymorphic function's contract
(is (monomorphise:elaborate
     (p:parse `(p:function ((p:function ((p:var (a)) (p:base (b) * nil))
                                        ((p:var (a))))
                            (p:var (a))
                            (p:box (p:base (b) * (>= v 0))))
                           ((p:var (a)))))
     ;; first argument to the first-class function is the non-negative accumulator,
     ;;  second argument is a strictly positive integer.
     ;; the second argument is the initial accumulator, which is *not* non-negative.
     ;; the third argument is a box (e.g., list) of not-too-negative integers.
     (m:parse `((m:function ((m:base integer (>= v 0))
                             (m:base integer (>= v 1)))
                            ((m:base integer (and (= v (+ (@- 0) (@- 1)))
                                                  (> v 0)
                                                  (> v (@- 0))))))
                (m:base integer true)
                (m:box (m:base integer (>= v -1)))))
     (p:parse `((p:base (z) integer true))))
    ;; we expect a function that accepts either:
    ;;   - the fold's result type, the sum of an arbitrary integer for the accumulator,
    ;;                                    and an iteration variable >= 0,
    ;;   - or the initial seed argument
    ;;
    ;; and an iteration variable that's the intersection of what the polymorphic
    ;; function expects and what the caller provides
    ;;
    ;; we also expect an arbitrary integer for the seed, and a list of
    ;; iteration values that satisfy both the argument and the
    ;; polymorphic function.
    ;;
    ;; we will return either the seed argument, or the result of
    ;; executing the loop, with an arbitrary accumulator.
    (m:parse
     `(m:function ((m:function ((m:base integer (or
                                                 (exists
                                                  ((f:a7 integer) (f:a8 integer))
                                                  (and (= v (+ f:a7 f:a8)) (> v 0)
                                                       (> v f:a7)
                                                       (let ((v f:a8))
                                                         (and (>= v -1)
                                                              (>= v 0)))))
                                                 (= v (@- 1 1))))
                                (m:base integer (and (>= v -1)
                                                     (>= v 0))))
                               ((m:base integer (and (= v (+ (@- 0) (@- 1)))
                                                     (> v 0)
                                                     (> v (@- 0))))))
                   (m:base integer true)
                   (m:box (m:base integer (and (>= v -1)
                                               (>= v 0)))))
                  ((m:base integer (or
                                    (exists ((f:a9 integer) (f:a10 integer))
                                            (and (= v (+ f:a9 f:a10)) (> v 0) (> v f:a9)
                                                 (let ((v f:a10))
                                                   (and (>= v -1) (>= v 0)))))
                                    (= v (@- 1))))))))

(finalize)
