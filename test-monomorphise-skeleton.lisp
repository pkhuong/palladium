(defpackage "TEST-MONOMORPHISE-SKELETON"
  (:use "CL" "PROVE")
  (:local-nicknames ("C" "CONDITION")
                    ("F" "FRESH")
                    ("M" "MONO-TYPE")
                    ("S" "SKELETON-TYPE")
                    ("P" "POLY-TYPE")
                    ("CONTRACT" "GATHER-POLYMORPHIC-CONTRACT")
                    ("MONOMORPHISE" "MONOMORPHISE-SKELETON")))

(in-package "TEST-MONOMORPHISE-SKELETON")

(setf prove:*default-test-function* #'equalp)

(plan 6)

;;; (a -> b) -> (b -> c) -> a -> c, with the first argument dec, and
;;; the second int, should derive c = a.

(defparameter *poly* (p:parse
                      `(p:function ((p:function ((p:var (a)))
                                                ((p:var (b))))
                                    (p:function ((p:var (b)))
                                                ((p:var (c)))))
                                   ((p:function ((p:var (a)))
                                                ((p:var (b))))))))

(defparameter *mono* (m:parse
                      `((m:function ((m:base integer (>= c:v 0)))
                                    ((m:base integer (= c:v (- (c:@- 0) 1)))))
                        (m:function ((m:base integer c:true))
                                    ((m:base integer (= c:v (+ (c:@- 0) 1))))))))

(defparameter *return* (p:parse `((p:function ((p:base (x) * nil))
                                              ((p:base (y) * nil))))))

(let ((c:*counter* 0)
      skeleton
      sort-solution
      flow-info
      conditions
      contract)
  (is (setf skeleton (poly-to-skeleton:convert *poly* *mono*))
      (s:parse `(s:function ((s:function ((s:base f:b1 + (f:t0)))
                                         ((s:base f:b3 - (f:t2))))
                             (s:function ((s:base f:b4 + (f:t2)))
                                         ((s:base f:b6 - (f:t5)))))
                            ((s:function ((s:base f:b7 - (f:t0)))
                                         ((s:base f:b8 + (f:t2))))))))
  (isnt (setf sort-solution
              (solve-sort-constraints:solution skeleton *poly* *mono* *return*))
        nil)
  (isnt (setf flow-info
              (gather-skeleton-flow-info:info skeleton sort-solution))
        nil)
  (isnt (setf conditions
              (let ((conditions (gather-argument-conditions:conditions
                                 *poly* skeleton *mono* *return*)))
                (and conditions
                     (backfill-argument-conditions:backfill skeleton flow-info conditions))))
        nil)
  (isnt (setf contract
              (contract:contract skeleton *poly*))
        nil)

  (is (monomorphise:monomorphise skeleton flow-info conditions contract)
      (m:parse `(m:function
                 ((m:function ((m:base integer (>= c:v 0)))
                              ((m:base integer (= c:v (- (c:@- 0) 1)))))
                  (m:function ((m:base integer (c:exists ((f:a9 integer))
                                                         (and (= c:v (- f:a9 1))
                                                              (let ((c:v f:a9))
                                                                (>= c:v 0))))))
                              ((m:base integer (= c:v (+ (c:@- 0) 1))))))
                 ((m:function ((m:base integer (>= c:v 0)))
                              ((m:base integer (c:exists ((f:a10 integer))
                                                         (and (= c:v (- f:a10 1))
                                                              (let ((c:v f:a10))
                                                                (= c:v (c:@- 0)))))))))))))

(finalize)
