;;; Solve the network of equality constraints on flow for base type
;;; variables.
;;;
;;; Incidentally, checks everything for structural compatibility.
(defpackage "SOLVE-SORT-CONSTRAINTS"
  (:use "CL")
  (:export "SOLUTION" "SORT-OR-NULL" "FOREACH-FLOW-VAR" "DO-FLOW-VARS")
  (:import-from "COERCE-TO-FUN" "COERCE-TO-FUN")
  (:local-nicknames ("UF" "ATTRIBUTED-DISJOINT-SET")
                    ("S" "SKELETON-TYPE")
                    ("M" "MONO-TYPE")
                    ("P" "POLY-TYPE")))

(in-package "SOLVE-SORT-CONSTRAINTS")

;;; Solution representation
(defvar *solution*)
(declaim (type uf:map *solution*))

(defstruct solution
  ;; keys are flow symbols.
  (map (uf:map (lambda (x y)
                 ;; values are sorts, and never wildcards
                 (check-type x (and symbol (not (member nil *))))
                 (check-type y (and symbol (not (member nil *))))
                 (unless (eql x y)
                   (throw 'fail (format nil "Unable to unify sorts ~S ~S." x y)))
                 x))
       :type uf:map
       :read-only t))

;;; Public readers
(defun sort-or-null (solution flow-key)
  (check-type solution solution)
  (check-type flow-key symbol)
  (values (uf:find (solution-map solution) flow-key)))

(defun foreach-flow-var (solution function &key sort)
  (let ((function (coerce-to-fun function)))
    (uf:foreach-element (solution-map solution)
                        (lambda (key value has-value)
                          (declare (ignore has-value))
                          (funcall function key value))
                        :sort sort)))

(defmacro do-flow-vars (((flow-var sort-or-null) solution result &key sort)
                        &body body)
  `(block nil
     (foreach-flow-var ,solution (lambda (,flow-var ,sort-or-null)
                                   ,@body)
                       :sort ,sort)
     ,result))

;;; traverse subskeletons that match against polymorphic variables to
;;; gather constraints.
(defgeneric %traverse (skeleton))

(defmethod %traverse ((skel s:function))
  (destructuring-bind (args results)
      (s:split skel)
    (mapcar #'%traverse args)
    (mapcar #'%traverse results)))

(defmethod %traverse ((skel s:box))
  (destructuring-bind (contents)
      (s:split skel)
    (%traverse contents)))

(defmethod %traverse ((skel s:base))
  (destructuring-bind (name polarity flow)
      (s:split skel)
    (declare (ignore name polarity))
    (uf:observe-equivalence *solution* flow)))

;;; Actual constraint gathering code.
(defgeneric %match (skeleton source))

(defun match-lists (skeletons sources)
  (assert (every #'s:type-p skeletons))
  (assert (every (lambda (source)
                   (or (m:type-p source)
                       (p:type-p source)))
                 sources))
  (unless (alexandria:length= skeletons sources)
    (throw 'fail (format nil "Unable to unify ~A with ~A (length mismatch)."
                         skeletons sources)))
  (map nil #'%match skeletons sources))

(defmethod %match (skel source)
  (throw 'fail (format nil "Missing case for ~A ~A." skel source)))

(defmethod %match ((skel s:function) (poly p:function))
  (destructuring-bind (skel-args skel-results)
      (s:split skel)
    (destructuring-bind (poly-args poly-results)
        (p:split poly)
      (match-lists skel-args poly-args)
      (match-lists skel-results poly-results))))

(defmethod %match ((skel s:box) (poly p:box))
  (destructuring-bind (skel-contents)
      (s:split skel)
    (destructuring-bind (poly-contents)
        (p:split poly)
      (%match skel-contents poly-contents))))

(defmethod %match ((skel s:base) (poly p:base))
  (destructuring-bind (flow sort condition)
      (p:split poly)
    (declare (ignore flow condition))
    (destructuring-bind (name polarity flow)
        (s:split skel)
      (declare (ignore name polarity))
      (assert flow)
      (if (eql sort '*)
          (uf:observe-equivalence *solution* flow)
          (uf:observe-equivalence *solution* flow :attribute sort))))
  nil)

(defmethod %match ((skel s:type) (poly p:var))
  (%traverse skel))

(defmethod %match ((skel s:function) (mono m:function))
  (destructuring-bind (skel-args skel-results)
      (s:split skel)
    (destructuring-bind (mono-args mono-results)
        (m:split mono)
      (match-lists skel-args mono-args)
      (match-lists skel-results mono-results))))

(defmethod %match ((skel s:box) (mono m:box))
  (destructuring-bind (skel-contents)
      (s:split skel)
    (destructuring-bind (mono-contents)
        (m:split mono)
      (%match skel-contents mono-contents))))

(defmethod %match ((skel s:base) (mono m:base))
  (destructuring-bind (sort condition)
      (m:split mono)
    (declare (ignore condition))
    (destructuring-bind (name polarity flow)
        (s:split skel)
      (declare (ignore name polarity))
      (assert flow)
      (uf:observe-equivalence *solution* flow :attribute sort))))

(defun solution (skeleton polymorphic-function monotypes polymorphic-results)
  (declare (type s:function)
           (type p:function polymorphic-function))
  (assert (every #'m:type-p monotypes))
  (assert (every #'p:type-p polymorphic-results))
  (flet ((solve ()
           (let* ((result (make-solution))
                  (*solution* (solution-map result)))
             (%match skeleton polymorphic-function)
             (destructuring-bind (skel-args skel-results)
                 (s:split skeleton)
               (match-lists skel-args monotypes)
               (match-lists skel-results polymorphic-results))
             (uf:do-elements ((flow sort has-value) *solution* result :sort #'string<)
               (declare (ignore has-value))
               (unless sort
                 (throw 'fail (format nil "No sort found for flow var ~A." flow)))))))
    (format *error-output*
            "Failed to find sort solution for ~A ~A ~A ~A: ~A"
            skeleton polymorphic-function monotypes polymorphic-results
            (catch 'fail (return-from solution (solve))))
    nil))
