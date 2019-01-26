;;; Causality tracking for skeleton types. Tracks all skeleton "base"
;;; types that transitively appeared in arguments currently in scope.
(defpackage "CAUSALITY"
  (:export "WITH-CAUSALITY-TRACKING" "WITH-CAUSES" "AVAILABLE-P")
  (:use "CL")
  (:local-nicknames ("S" "SKELETON-TYPE")))

(in-package "CAUSALITY")
;;; list of s:base names.
(defvar *available-causes*)

(defgeneric %collect (skel)
  (:documentation "Traverses a skeleton and recursively pushes the
  names of all s:base to *available-causes*."))

(defun collect-all (skels)
  (map nil #'%collect skels))

(defmethod %collect ((skel s:function))
  (destructuring-bind (args res)
      (s:split skel)
    (collect-all args)
    (collect-all res)))

(defmethod %collect ((skel s:box))
  (destructuring-bind (contents)
      (s:split skel)
    (%collect contents)))

(defmethod %collect ((skel s:base))
  (destructuring-bind (name polarity flow position)
      (s:split skel)
    (declare (ignore polarity flow position))
    (push name *available-causes*)))

(defun %call-with-causality-tracking (thunk)
  (let ((*available-causes* '()))
    (funcall thunk)))

(defmacro with-causality-tracking (() &body body)
  `(%call-with-causality-tracking (lambda () ,@body)))

(defun %call-with-causes (skeletons thunk)
  (let ((*available-causes* *available-causes*))
    (collect-all skeletons)
    (funcall thunk)))

(defmacro with-causes ((causes) &body body)
  `(%call-with-causes ,causes (lambda () ,@body)))

(defun available-p (base)
  (check-type base s:base)
  (destructuring-bind (name polarity flow position)
      (s:split base)
    (declare (ignore polarity position flow position))
    (find name *available-causes*)))
