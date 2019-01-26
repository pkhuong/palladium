(defpackage "TEST-RETURN-PATTERN-TO-POLY"
  (:use "CL" "PROVE")
  (:local-nicknames ("C" "CONDITION")
                    ("F" "FRESH")
                    ("IN" "PATTERN-TYPE")
                    ("OUT" "POLY-TYPE")
                    ("S" "SKELETON-TYPE")))

(in-package "TEST-RETURN-PATTERN-TO-POLY")

(setf prove:*default-test-function* #'equalp)

(plan 6)

(let ((c:*counter* 0))
  (is (return-pattern-to-poly:convert (in:parse `((in:base (a) c:* c:true)))
                                      (s:parse `((s:base f:b0 + (f:t0) :res))))
      (out:parse `((out:base (a) c:* c:true)))))

(let ((c:*counter* 0))
  (is (return-pattern-to-poly:convert (in:parse `((in:base (a) integer c:true)))
                                      (s:parse `((s:base f:b0 + (f:t0) :res))))
      (out:parse `((out:base (a) integer c:true)))))

(let ((c:*counter* 0))
  (is (return-pattern-to-poly:convert (in:parse ` ((in:var (a))
                                                   (in:var (b))
                                                   (in:spread (c))
                                                   (in:base (d) * nil)))
                                      (s:parse `((s:base f:b0 + (f:t0) :res)
                                                 (s:function () ())
                                                 (s:function () ())
                                                 (s:function () ())
                                                 (s:base f:b0 + (f:t0) :res))))
      (out:parse `((out:var (a))
                   (out:var (b))
                   (out:var (f:s0))
                   (out:var (f:s1))
                   (out:base (d) * nil)))))

(let ((c:*counter* 0))
  (is (return-pattern-to-poly:convert (in:parse `((in:function () ((in:spread (a))))
                                                  (in:function () ((in:spread (b))))))
                                      (s:parse `((s:function () ((s:base f:b0 + (f:t0) :res)))
                                                 (s:function () ((s:base f:b0 + (f:t0) :res))))))
      (out:parse `((out:function () ((out:var (f:s0))))
                   (out:function () ((out:var (f:s1))))))))

(let ((c:*counter* 0))
  (is (return-pattern-to-poly:convert (in:parse `((in:function () ((in:spread (a))))
                                                  (in:function () ((in:spread (a))))))
                                      (s:parse `((s:function () ((s:base f:b0 + (f:t0) :res)))
                                                 (s:function () ((s:base f:b0 + (f:t0) :res))))))
      (out:parse `((out:function () ((out:var (f:s0))))
                   (out:function () ((out:var (f:s0))))))))

(let ((c:*counter* 0))
  (is (return-pattern-to-poly:convert (in:parse `((in:box
                                                   (in:function ((in:spread (a)))
                                                                ((in:spread (b)))))
                                                  (in:function ((in:spread (b)))
                                                               ((in:spread (c))))))
                                      (s:parse `((s:box
                                                  (s:function ((s:base f:b0 + (f:t0) :arg))
                                                              ((s:base f:b0 + (f:t0) :res))))
                                                 (s:function ((s:base f:b0 + (f:t0) :arg))
                                                             ((s:base f:b0 + (f:t0) :res))))))
      (out:parse `((out:box
                    (out:function ((out:var (f:s0)))
                                  ((out:var (f:s1)))))
                   (out:function ((out:var (f:s1)))
                                 ((out:var (f:s2))))))))

(finalize)
