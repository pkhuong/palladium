;;; Eliminates type variables in poly types.
;;;
;;; The resulting skeleton function type only tracks subtyping at the
;;; base type level.
;;;
;;; As an implementation accident, polymorphic type variables live in
;;; their own namespace.
(defpackage "POLY-TO-SKELETON"
  (:use "CL")
  (:local-nicknames ("C" "CONDITION")
                    ("M" "MONO-TYPE")
                    ("IN" "POLY-TYPE")
                    ("OUT" "SKELETON-TYPE"))
  (:export "CONVERT"))

(in-package "POLY-TO-SKELETON")

;;; Toplevel folds: to-skeleton, merge-skeletons, ensure-polarity
(defgeneric to-skeleton (mono)
  (:documentation "Returns a skeleton that matches mono in positive polarity."))

(defgeneric merge-skeletons (x y)
  (:documentation "Returns a skeleton with the pairwise union of the
  flow variables in x y."))

(defgeneric ensure-polarity (skeleton polarity)
  (:documentation "Returns a copy of the skeleton with fixed up polarity."))

(defun negate (polarity)
  (ecase polarity
    (+ '-)
    (- '+)))

;;; Implementation of to-skeleton
(defun list-to-skeleton (mono-types)
  (assert (every #'m:type-p mono-types))
  (mapcar #'to-skeleton  mono-types))

(defmethod to-skeleton ((mono m:function))
  (destructuring-bind (mono-args mono-results)
      (m:split mono)
    (out:function (list-to-skeleton mono-args)
                  (list-to-skeleton mono-results))))

(defmethod to-skeleton ((mono m:base))
  ;; pass in an explicit name because it'll get renamed.
  (out:base 'fresh:b0 '+ (list (c:fresh '#:t))))

(defmethod to-skeleton ((mono m:box))
  (destructuring-bind (contents)
      (m:split mono)
    (out:box (to-skeleton contents))))

;;; Implementation of merge-skeleton
(defmethod merge-skeletons (x y)
  (throw 'fail (format nil "Unable to merge skeletons ~A ~A." x y)))

(defun merge-skeleton-lists (xs ys)
  (unless (alexandria:length= xs ys)
    (throw 'fail (format nil "Unable to merge skeleton lists ~A ~A (lengths differ)."
                         xs ys)))
  (mapcar #'merge-skeletons xs ys))

(defmethod merge-skeletons ((x out:function) (y out:function))
  (destructuring-bind (x-arguments x-results)
      (out:split x)
    (destructuring-bind (y-arguments y-results)
        (out:split y)
      (out:function (merge-skeleton-lists x-arguments y-arguments)
                    (merge-skeleton-lists x-results y-results)))))

(defmethod merge-skeletons ((x out:box) (y out:box))
  (destructuring-bind (x-contents)
      (out:split x)
    (destructuring-bind (y-contents)
        (out:split y)
      (out:box (merge-skeletons x-contents y-contents)))))

(defmethod merge-skeletons ((x out:base) (y out:base))
  (destructuring-bind (x-name x-polarity x-flow)
      (out:split x)
    (declare (ignore x-name))
    (destructuring-bind (y-name y-polarity y-flow)
        (out:split y)
      (declare (ignore y-name))
      (assert (eql x-polarity '+))
      (assert (eql y-polarity '+))
      (let ((flows (ordered:set)))
        (ordered:record-all flows x-flow)
        (ordered:record-all flows y-flow)
        ;; we don't need real base location names here.
        (out:base 'fresh:b0 '+ (ordered:entries flows))))))

;;; Implementation of ensure-polarity
(defun ensure-polarity-list (skeletons polarity)
  (mapcar (lambda (skeleton)
            (ensure-polarity skeleton polarity))
          skeletons))

(defmethod ensure-polarity ((skel out:function) polarity)
  (destructuring-bind (arguments results)
      (out:split skel)
    (out:function (ensure-polarity-list arguments (negate polarity))
                  (ensure-polarity-list results polarity))))

(defmethod ensure-polarity ((skel out:box) polarity)
  (destructuring-bind (contents)
      (out:split skel)
    (out:box (ensure-polarity contents polarity))))

(defmethod ensure-polarity ((skel out:base) polarity)
  (destructuring-bind (name skel-polarity flow)
      (out:split skel)
    (declare (ignore name skel-polarity))
    (out:base (c:fresh '#:b) polarity flow)))

;;; Intern or find preexisting extensions for tvars

;; hash table of flow var to skeleton expansion, in +ve polarity.
(defvar *expansion-cache*)
(declaim (type hash-table *expansion-cache*))

(defun find-or-create-expansion (flow mono)
  (check-type flow symbol)
  (check-type mono m:type)
  (or (gethash flow *expansion-cache*)
      (setf (gethash flow *expansion-cache*)
            (to-skeleton mono))))

(defun find-expansion-or-fail (flow)
  (check-type flow symbol)
  (or (gethash flow *expansion-cache*)
      (throw 'fail (format nil "Unable to find expansion for ~A." flow))))

;;; %convert poly mono polarity -> skeleton
(defgeneric %convert (poly mono polarity))

(defun convert-list (poly-types mono-types polarity)
  (assert (every #'in:type-p poly-types))
  (assert (every #'m:type-p mono-types))
  (check-type polarity (member + -))
  (unless (alexandria:length= poly-types mono-types)
    (throw 'fail "Mismatching type list size."))
  (mapcar (lambda (poly mono)
            (%convert poly mono polarity))
          poly-types mono-types))

(defmethod %convert (poly mono polarity)
  (throw 'fail (format nil "Missing case for ~A ~A" poly mono)))

(defmethod %convert ((poly in:function) (mono m:function) polarity)
  (destructuring-bind (poly-args poly-results)
      (in:split poly)
    (destructuring-bind (mono-args mono-results)
        (m:split mono)
      (out:function (convert-list poly-args mono-args (negate polarity))
                    (convert-list poly-results mono-results polarity)))))

(defmethod %convert ((poly in:box) (mono m:box) polarity)
  (destructuring-bind (poly-contents)
      (in:split poly)
    (destructuring-bind (mono-contents)
        (m:split mono)
      (out:box (%convert poly-contents mono-contents polarity)))))

(defmethod %convert ((poly in:base) (mono m:base) polarity)
  (destructuring-bind (flow sort condition)
      (in:split poly)
    (declare (ignore sort condition))
    (out:base (c:fresh '#:b) polarity flow)))

(defmethod %convert ((poly in:var) (mono m:type) polarity)
  (destructuring-bind (flow)
      (in:split poly)
    (check-type flow (cons symbol))
    (let ((merged (reduce #'merge-skeletons flow
                          :key (lambda (flow-var)
                                 (find-or-create-expansion flow-var mono)))))
      ;; This call to ensure polarity is crucial because it not only
      ;; fixes up polarity annotations, but also re-generate fresh
      ;; names for every base variable.
      (ensure-polarity merged polarity))))

;;; %expand poly polarity -> skeleton
(defgeneric %expand (poly polarity))

(defun expand-list (poly-types polarity)
  (mapcar (lambda (poly)
            (%expand poly polarity))
          poly-types))

(defmethod %expand ((poly in:function) polarity)
  (destructuring-bind (poly-args poly-results)
      (in:split poly)
    (out:function (expand-list poly-args (negate polarity))
                  (expand-list poly-results polarity))))

(defmethod %expand ((poly in:box) polarity)
  (destructuring-bind (contents)
      (in:split poly)
    (out:box (%expand contents polarity))))

(defmethod %expand ((poly in:base) polarity)
  (destructuring-bind (flow sort condition)
      (in:split poly)
    (declare (ignore sort condition))
    (out:base (c:fresh '#:b) polarity flow)))

(defmethod %expand ((poly in:var) polarity)
  (destructuring-bind (flow)
      (in:split poly)
    (check-type flow (cons symbol))
    (let ((merged (reduce #'merge-skeletons flow
                          :key (lambda (flow-var)
                                 (find-expansion-or-fail flow-var)))))
      (ensure-polarity merged polarity))))

(defun convert (poly mono-types)
  (check-type poly in:function)
  (assert (every #'m:type-p mono-types))
  (let ((*expansion-cache* (make-hash-table)))
    (flet ((convert ()
             (destructuring-bind (arguments results)
                 (in:split poly)
               (out:function (convert-list arguments mono-types '-)
                             (expand-list results '+)))))
      (format *error-output*
              "Failed in ~S: ~A~%"
              'convert
              (catch 'fail (return-from convert (convert))))
      nil)))
