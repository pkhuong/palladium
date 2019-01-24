(defpackage "TEST-POLY-TO-SKELETON"
  (:use "CL" "PROVE")
  (:local-nicknames ("C" "CONDITION")
                    ("F" "FRESH")
                    ("IN" "POLY-TYPE")
                    ("OUT" "SKELETON-TYPE")
                    ("M" "MONO-TYPE")))

(in-package "TEST-POLY-TO-SKELETON")

(setf prove:*default-test-function* #'equalp)

(plan 4)

(let ((c:*counter* 0))
  (is (poly-to-skeleton:convert (in:parse `(in:function ((in:var (a))
                                                         (in:var (b)))
                                                        ((in:var (a b)))))
                                (m:parse `((m:box (m:base integer c:true))
                                           (m:box (m:base integer c:true)))))
      (out:parse `(out:function ((out:box (out:base f:b1 - (f:t0)))
                                 (out:box (out:base f:b3 - (f:t2))))
                                ((out:box (out:base f:b4 + (f:t0 f:t2))))))))

(let ((c:*counter* 0))
  (is (poly-to-skeleton:convert (in:parse `(in:function ((in:var (a))
                                                         (in:base (b) c:* c:true))
                                                        ((in:var (a)))))
                                (m:parse `((m:box (m:base integer c:true))
                                           (m:base integer c:true))))
      (out:parse `(out:function ((out:box (out:base f:b1 - (f:t0)))
                                 (out:base f:b2 - (b)))
                                ((out:box (out:base f:b3 + (f:t0))))))))

(let ((c:*counter* 0))
  (is (poly-to-skeleton:convert (in:parse `(in:function ((in:var (a))
                                                         (in:var (b)))
                                                        ((in:var (a b)))))
                                (m:parse `((m:function ((m:base integer c:true))
                                                       ((m:base integer c:true)))
                                           (m:function ((m:base integer c:false))
                                                       ((m:base integer c:false))))))
      (out:parse `(out:function ((out:function ((out:base f:b2 + (f:t0)))
                                               ((out:base f:b3 - (f:t1))))
                                 (out:function ((out:base f:b6 + (f:t4)))
                                               ((out:base f:b7 - (f:t5)))))
                                ((out:function ((out:base f:b8 - (f:t0 f:t4)))
                                               ((out:base f:b9 + (f:t1 f:t5)))))))))

(let ((c:*counter* 0))
  (is (poly-to-skeleton:convert (in:parse `(in:function ((in:box
                                                          (in:function ((in:var (a)))
                                                                       ((in:var (b)))))
                                                        (in:function ((in:var (b)))
                                                                     ((in:base (c) integer c:true))))
                                                       ((in:box
                                                         (in:function ((in:var (a)))
                                                                      ((in:base (c) * c:false)))))))
                               (m:parse `((m:box
                                           (m:function ((m:base integer c:true))
                                                       ((m:base integer (= c:v (+ (@- 0) 1))))))
                                          (m:function ((m:base integer c:true))
                                                      ((m:base integer (= c:v (- (@- 0) 1))))))))
      (out:parse `(out:function ((out:box
                                  (out:function ((out:base f:b1 + (f:t0)))
                                                ((out:base f:b3 - (f:t2)))))
                                 (out:function ((out:base f:b4 + (f:t2)))
                                               ((out:base f:b5 - (c)))))
                                ((out:box
                                  (out:function ((out:base f:b6 - (f:t0)))
                                                ((out:base f:b7 + (c))))))))))

(finalize)
