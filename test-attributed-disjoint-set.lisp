(defpackage "TEST-ATTRIBUTED-DISJOINT-SET"
  (:use "CL" "PROVE")
  (:local-nicknames ("UF" "ATTRIBUTED-DISJOINT-SET")))

(in-package "TEST-ATTRIBUTED-DISJOINT-SET")

(setf prove:*default-test-function* #'equalp)

(plan 20)

;;; get / set / get
(let ((map (uf:map (lambda (x y)
                     (assert (equalp x y))
                     x))))
  (uf:observe-equivalence map '() :attribute 'z)
  (is-values (uf:find map 'x) '(nil nil))
  (uf:observe-equivalence map '(x) :attribute 'y)
  (is-values (uf:find map 'x) '(y t))
  (is-values (uf:find map 'y) '(nil nil)))

;;; equivalence / set / get
(let ((map (uf:map (lambda (x y)
                     (assert (equalp x y))
                     x))))
  (uf:observe-equivalence map '(x y))
  (is-values (uf:find map 'x) '(nil nil))
  (uf:observe-equivalence map '(y) :attribute t)
  (is-values (uf:find map 'x) '(t t))
  (is-values (uf:find map 'y) '(t t))
  (is-values (uf:find map 'z) '(nil nil)))

;;; set / equivalence / get
(let ((map (uf:map (lambda (x y)
                     (assert (equalp x y))
                     x))))
  (uf:observe-equivalence map '(x) :attribute t)
  (is-values (uf:find map 'x) `(t t))
  (uf:observe-equivalence map '(x y))
  (is-values (uf:find map 'x) '(t t))
  (is-values (uf:find map 'y) '(t t)))

;;; failed set / set / equivalence
(let ((map (uf:map (lambda (x y)
                     (unless (equalp x y)
                       (throw 'fail nil))
                     x))))
  (uf:observe-equivalence map '(x) :attribute 1)
  (uf:observe-equivalence map '(y) :attribute 2)
  (is (catch 'fail
        (uf:observe-equivalence map '(x y))
        t)
      nil))

;;; successful set / set / equivalence
(let ((map (uf:map (lambda (x y)
                     (unless (equalp x y)
                       (throw 'fail nil))
                     x))))
  (uf:observe-equivalence map '(x) :attribute 1)
  (uf:observe-equivalence map '(y) :attribute 1)
  (is (catch 'fail
        (uf:observe-equivalence map '(x y))
        t)
      t)
  (is-values (uf:find map 'x) '(1 t))
  (is-values (uf:find map 'y) '(1 t)))

;; failed equivalence / set / set
(let ((map (uf:map (lambda (x y)
                     (unless (equalp x y)
                       (throw 'fail nil))
                     x))))
  (uf:observe-equivalence map '(x y) :attribute 1)
  (is (catch 'fail
        (uf:observe-equivalence map '(y) :attribute 2)
        t)
      nil))

;; successful equivalence / set / set
(let ((map (uf:map (lambda (x y)
                     (unless (equalp x y)
                       (throw 'fail nil))
                     x))))
  (uf:observe-equivalence map '(x y) :attribute 1)
  (is (catch 'fail
        (uf:observe-equivalence map '(y) :attribute 1)
        t)
      t)
  (is-values (uf:find map 'x) '(1 t))
  (is-values (uf:find map 'y) '(1 t)))

;;; iterate over values
(let ((map (uf:map (lambda (x y)
                     (assert (equalp x y))
                     x))))
  (uf:observe-equivalence map '(x y) :attribute 1)
  (uf:observe-equivalence map '(z))
  (let ((elements '()))
    (is (uf:do-elements ((key value has-value) map 42 :sort 'string<)
          (push (list key value has-value) elements))
        42)
    (is (reverse elements)
        '((x 1 t)
          (y 1 t)
          (z nil nil)))))

(finalize)
