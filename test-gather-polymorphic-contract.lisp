(defpackage "TEST-GATHER-POLYMORPHIC-CONTRACT"
  (:use "CL" "PROVE")
  (:local-nicknames ("C" "CONDITION")
                    ("F" "FRESH")
                    ("S" "SKELETON-TYPE")
                    ("P" "POLY-TYPE")
                    ("CONTRACT" "GATHER-POLYMORPHIC-CONTRACT")))

(in-package "TEST-GATHER-POLYMORPHIC-CONTRACT")

(setf prove:*default-test-function* #'equalp)

(plan 8)

;;; a -> int: the result is a full solution.
(let ((skel (s:parse `(s:function ((s:box (s:base f:b0 - (a))))
                                  ((s:base f:b1 + (b))))))
      (poly (p:parse `(p:function ((p:var (a)))
                                  ((p:base (b) integer (= c:v 42))))))
      (b0 (s:parse `(s:base f:b0 - (a))))
      (b1 (s:parse `(s:base f:b1 + (b))))
      contract)
  (isnt (setf contract (contract:contract skel poly)) nil)
  (is (contract:solution-or-nil contract b1)
      `(= c:v 42))
  (is (contract:constraint contract b0) 'c:true)
  (is (contract:constraint contract b1) 'c:true))

;;; gather constraints when in negative position or the sort is left
;;; unspecified.
(let ((skel (s:parse `(s:function ((s:base f:b0 - (a))
                                   (s:box (s:base f:b1 - (b))))
                                  ((s:base f:b2 + (b))))))
      (poly (p:parse `(p:function ((p:base (a) integer (> c:v 0))
                                   (p:box (p:base (b) * nil)))
                                  ((p:base (c) * (> c:v (c:@- 0)))))))
      (b0 (s:parse `(s:base f:b0 - (a))))
      (b1 (s:parse `(s:base f:b1 - (b))))
      (b2 (s:parse `(s:base f:b2 + (b))))
      contract)
  (isnt (setf contract (contract:contract skel poly)) nil)
  (is (contract:solution-or-nil contract b2) nil)
  (is (contract:constraint contract b0) `(> c:v 0))
  (is (contract:constraint contract b1) 'c:true)
  (is (contract:constraint contract b2) `(> c:v ,b0)))

(finalize)
