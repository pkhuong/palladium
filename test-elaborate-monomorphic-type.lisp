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

(plan 9)

;;; Driving example: derive dataflow for the composition (.) operator.
(is (monomorphise:elaborate
     ;; this polymorphic accepts a first function, from any number of
     ;; values to any number of values (functions are tuple->tuple,
     ;; not curried), and another function that may accept the first
     ;; function's results and returns an arbitrary number of values.
     ;;
     ;; the result is a function that accepts what the first argument
     ;; accepts, and returns what the second returns.
     (p:parse `(p:function ((p:function ((p:spread (a)))
                                        ((p:spread (b))))
                            (p:function ((p:spread (b)))
                                        ((p:spread (c)))))
                           ((p:function ((p:spread (a)))
                                        ((p:spread (c)))))))
     ;; pass in `dec` for non-negative integers, and `inc` for
     ;; arbitrary integers.
     (m:parse `((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (- (@- 0) 1)))))
                (m:function ((m:base integer true))
                            ((m:base integer (= v (+ (@- 0) 1)))))))
     ;; expect anything
     (p:parse `((p:spread (a)))))
    ;; expected result:
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

;;; we have to be more conservative if the function may be impure.
(is (monomorphise:elaborate
     ;; accept a function from a to b, and return a function from a to b.
     ;; without purity, we can't assume we know this is the identify.
     ;; the returnee could, e.g,, be a caching wrapper of the argument.
     (p:parse `(p:function ((p:function ((p:spread (a)))
                                        ((p:spread (b)))))
                           ((p:function ((p:spread (a)))
                                        ((p:spread (b)))))))
     ;; pass in an identify function
     (m:parse `((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (@- 0)))))))
     ;; expect anything
     (p:parse `((p:spread (a))))
     :assume-purity nil)
    ;; expected result:
    ;;    ({int a : v >= 0} -> {int : v = a})
    ;; -> ({int a : v >= 0} -> {int : v >= 0 })
    (m:parse `(m:function
               ((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (@- 0))))))
               ((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (exists ((f:a9 integer))
                                                     (and (= v f:a9)
                                                          (let ((v f:a9))
                                                            (>= v 0)))))))))))

;;; we can still do some useful things, even if there may be side effects.
(is (monomorphise:elaborate
     ;; accept an `a`, and a function from a to b, return a b and a
     ;; function from a to be.
     ;;
     ;; the returned function may not be the same as the second
     ;; argument, but we know exactly where the first argument comes
     ;; from, purity or no.
     (p:parse `(p:function ((p:spread (a))
                            (p:function ((p:spread (a)))
                                        ((p:spread (b)))))
                           ((p:spread (b))
                            (p:function ((p:spread (a)))
                                        ((p:spread (b)))))))
     (m:parse `((m:base integer (= v 42))
                (m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (@- 0)))))))
     ;; expect anything
     (p:parse `((p:spread (a))))
     :assume-purity nil)
    ;; expected result:
    ;;    {int a : v >= 0} -> ({int b : v >= 0} -> {int : v = b})
    ;; -> a, ({int c : v >= 0} -> {int : v >= 0 })
    (m:parse `(m:function
               ((m:base integer (= v 42))
                (m:function ((m:base integer (or (= v (@- 0 1)) (>= v 0))))
                            ((m:base integer (= v (@- 0))))))
               ((m:base integer (exists ((f:a12 integer))
                                        (and (= v f:a12)
                                             (let ((v f:a12))
                                               (= v (@- 0))))))
                (m:function ((m:base integer (>= v 0)))
                            ((m:base integer (exists ((f:a13 integer))
                                                     (and (= v f:a13)
                                                          (let ((v f:a13))
                                                            (or (= v (@- 0 1))
                                                                (>= v 0))))))))))))

;;; Given the assumption of purity, we can assume causality and
;;; scoping match, so we can even be tighter when multiple sources of
;;; `a` are available, but in independent scopes.
(is (monomorphise:elaborate
     ;; this polymorphic accepts a first function, from any number of
     ;; values to any number of values (functions are tuple->tuple,
     ;; not curried), and another function that may accept the first
     ;; function's results and returns an arbitrary number of values.
     ;;
     ;; the result is a function that accepts what the first argument
     ;; accepts, and returns what the second returns.
     (p:parse `(p:function ((p:function ((p:spread (a)))
                                        ((p:spread (b))))
                            (p:function ((p:spread (b)))
                                        ((p:spread (c)))))
                           ((p:function ((p:spread (a)))
                                        ((p:spread (c))))
                            (p:function ((p:spread (a)))
                                        ((p:spread (c)))))))
     ;; pass in `dec` for non-negative integers, and `inc` for
     ;; arbitrary integers.
     (m:parse `((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (- (@- 0) 1)))))
                (m:function ((m:base integer true))
                            ((m:base integer (= v (+ (@- 0) 1)))))))
     ;; expect anything
     (p:parse `((p:spread (a)))))
    ;; expected result:
    ;;    ({int a : v >= 0} -> {int : v = a - 1})
    ;; -> ({int a : v = x - 1, x >= 0} -> {int : v = a + 1}
    ;; -> ({int a : v >= 0} -> {int : v = x + 1, x = a - 1}),
    ;;    ({int a : v >= 0} -> {int : v = x + 1, x = a - 1}),
    (m:parse `(m:function
               ((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (- (@- 0) 1)))))
                (m:function ((m:base integer (exists
                                              ((f:a16 integer))
                                              (and (= v (- f:a16 1))
                                                   (let ((v f:a16))
                                                     (>= v 0))))))
                            ((m:base integer (= v (+ (@- 0) 1))))))
               ((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (exists
                                              ((f:a17 integer))
                                              (and (= v (+ f:a17 1))
                                                   (let ((v f:a17))
                                                     (exists
                                                      ((f:a18 integer))
                                                      (and (= v (- f:a18 1))
                                                           (let ((v f:a18))
                                                             (= v (@- 0)))))))))))
                (m:function ((m:base integer (>= v 0)))
                            ((m:base integer (exists
                                              ((f:a19 integer))
                                              (and (= v (+ f:a19 1))
                                                   (let ((v f:a19))
                                                     (exists
                                                      ((f:a20 integer))
                                                      (and (= v (- f:a20 1))
                                                           (let ((v f:a20))
                                                             (= v (@- 0)))))))))))))))

(is (monomorphise:elaborate
     ;; this polymorphic accepts a first function, from any number of
     ;; values to any number of values (functions are tuple->tuple,
     ;; not curried), and another function that may accept the first
     ;; function's results and returns an arbitrary number of values.
     ;;
     ;; the result is a function that accepts what the first argument
     ;; accepts, and returns what the second returns.
     (p:parse `(p:function ((p:function ((p:spread (a)))
                                        ((p:spread (b))))
                            (p:function ((p:spread (b)))
                                        ((p:spread (c)))))
                           ((p:function ((p:spread (a)))
                                        ((p:spread (c)))))))
     ;; pass in `dec` for non-negative integers, and `inc` for
     ;; arbitrary integers.
     (m:parse `((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (- (@- 0) 1)))))
                (m:function ((m:base integer true))
                            ((m:base integer (= v (+ (@- 0) 1)))))))
     ;; expect anything
     (p:parse `((p:spread (a)))))
    ;; expected result:
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

;; similar, but with a tighter precondition on `inc`, and an explicit
;; precondition for the result.
(is (monomorphise:elaborate
     ;; this polymorphic accepts a first function, from any number of
     ;; values to any number of values (functions are tuple->tuple,
     ;; not curried), and another function that may accept the first
     ;; function's results and returns an arbitrary number of values.
     ;;
     ;; the result is a function that accepts what the first argument
     ;; accepts, and returns what the second returns.
     (p:parse `(p:function ((p:function ((p:spread (a)))
                                        ((p:spread (b))))
                            (p:function ((p:spread (b)))
                                        ((p:spread (c)))))
                           ((p:function ((p:spread (a)))
                                        ((p:spread (c)))))))
     ;; pass in `dec` for non-negative integers, and `inc` for
     ;; small integers.
     (m:parse `((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (- (@- 0) 1)))))
                (m:function ((m:base integer (< v 1000)))
                            ((m:base integer (= v (+ (@- 0) 1)))))))
     ;; expect a function that accepts something in [0, 1000] and returns anything.
     (p:parse `((p:function ((p:base (a) integer (and (>= v 0) (<= v 1000))))
                            ((p:spread (b)))))))
    ;; expected result:
    ;;    ({int a : 0 <= v <= 1000} -> {int : v = a - 1})
    ;; -> ({int a : v = x - 1, 0 <= x <= 1000} -> {int : v = a + 1}
    ;; -> ({int a : 0 <= v <= 1000} -> {int : v = x + 1, x = a - 1})
    (m:parse `(m:function
               ((m:function ((m:base integer (and (>= v 0) (<= v 1000))))
                            ((m:base integer (= v (- (@- 0) 1)))))
                (m:function ((m:base integer (exists ((f:a13 integer))
                                                     (and (= v (- f:a13 1))
                                                          (let ((v f:a13))
                                                            (and (>= v 0)
                                                                 (<= v 1000)))))))
                            ((m:base integer (= v (+ (@- 0) 1))))))
               ((m:function ((m:base integer (and (>= v 0) (<= v 1000))))
                            ((m:base integer (exists
                                              ((f:a14 integer))
                                              (and (= v (+ f:a14 1))
                                                   (let ((v f:a14))
                                                     (exists
                                                      ((f:a15 integer))
                                                      (and (= v (- f:a15 1))
                                                           (let ((v f:a15))
                                                             (= v (@- 0)))))))))))))))

;; same compose inc / dec example, but this time without an explicit
;; precondition on the returned function. This will yield something
;; that will not typecheck.
(is (monomorphise:elaborate
     ;; this polymorphic accepts a first function, from any number of
     ;; values to any number of values (functions are tuple->tuple,
     ;; not curried), and another function that may accept the first
     ;; function's results and returns an arbitrary number of values.
     ;;
     ;; the result is a function that accepts what the first argument
     ;; accepts, and returns what the second returns.
     (p:parse `(p:function ((p:function ((p:spread (a)))
                                        ((p:spread (b))))
                            (p:function ((p:spread (b)))
                                        ((p:spread (c)))))
                           ((p:function ((p:spread (a)))
                                        ((p:spread (c)))))))
     ;; pass in `dec` for non-negative integers, and `inc` for
     ;; small integers.
     (m:parse `((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (- (@- 0) 1)))))
                (m:function ((m:base integer (< v 1000)))
                            ((m:base integer (= v (+ (@- 0) 1)))))))
     ;; expect a function from anything to anything.
     (p:parse `((p:function ((p:spread (a)))
                            ((p:spread (b)))))))
    ;; expected result:
    ;;    ({int a : 0 <= v} -> {int : v = a - 1})
    ;; -> ({int a : v = x - 1, 0 <= x } -> {int : v = a + 1}
    ;; -> ({int a : 0 <= v} -> {int : v = x + 1, x = a - 1})
    ;;
    ;; While the first argument is satisfied by `dec`, the second
    ;; has too lax an argument type for `inc`.
    (m:parse `(m:function
               ((m:function ((m:base integer (>= v 0)))
                            ((m:base integer (= v (- (@- 0) 1)))))
                (m:function ((m:base integer
                                     (exists ((f:a14 integer))
                                             (and (= v (- f:a14 1))
                                                  (let ((v f:a14))
                                                    (>= v 0))))))
                            ((m:base integer (= v (+ (@- 0) 1))))))
               ((m:function ((m:base integer (>= v 0)))
                            ((m:base integer
                                     (exists ((f:a15 integer))
                                             (and (= v (+ f:a15 1))
                                                  (let ((v f:a15))
                                                    (exists ((f:a16 integer))
                                                            (and (= v (- f:a16 1))
                                                                 (let ((v f:a16))
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
