;;; Gather flow info at the skeleton base variable level.
(defpackage "GATHER-SKELETON-FLOW-INFO"
  (:use "CL")
  (:export "INFO" "ALL-BASES" "BASE-SORT" "BASE-SINKS" "BASE-SOURCES")
  (:local-nicknames ("S" "SKELETON-TYPE")
                    ("SOLUTION" "SOLVE-SORT-CONSTRAINTS")))

(in-package "GATHER-SKELETON-FLOW-INFO")

(defstruct info
  (bases (ordered:set) :type ordered:set :read-only t)
  ;; base name -> sort
  (base-sort (ordered:map) :type ordered:map :read-only t)
  ;; flow name -> ordered:set of negative base objects that flow into it.
  (flow-sources (ordered:map) :type ordered:map :read-only t)
  ;; flow name -> ordered:set of positive base objects that receive it.
  (flow-sinks (ordered:map) :type ordered:map :read-only t))

(defun all-bases (info)
  (declare (type info info))
  (ordered:entries (info-bases info)))

(defun base-sort (info base)
  (declare (type info info)
           (type s:base base))
  (destructuring-bind (name polarity flow)
      (s:split base)
    (declare (ignore polarity flow))
    (ordered:find (info-base-sort info) name)))

(defun base-sinks (info base)
  "Returns all the (positive) skeleton base where this negative
   skeleton base might flow."
  (declare (type info info)
           (type s:base base))
  (destructuring-bind (name polarity flow)
      (s:split base)
    (declare (ignore name))
    (assert (eql polarity '-))
    (let ((sinks (ordered:set :test #'equalp))
          (flow-sinks (info-flow-sinks info)))
      (dolist (var flow (ordered:entries sinks))
        (let ((var-sinks (ordered:find flow-sinks var)))
          (declare (type (or null ordered:set) var-sinks))
          (when var-sinks
            (ordered:record-all sinks (ordered:entries var-sinks))))))))

(defun base-sources (info base)
  "Returns all the negative skeleton bases that may flow to this
  positive skeleton base."
  (destructuring-bind (name polarity flow)
      (s:split base)
    (declare (ignore name))
    (assert (eql polarity '+))
    (let ((sources (ordered:set :test #'equalp))
          (flow-sources (info-flow-sources info)))
      (dolist (var flow (ordered:entries sources))
        (let ((var-sources (ordered:find flow-sources var)))
          (declare (type (or null ordered:set) var-sources))
          (when var-sources
            (ordered:record-all sources (ordered:entries var-sources))))))))

(defvar *info*)
(declaim (type info *info*))

(defun register-sort (base-name sort)
  (declare (type (and symbol (not (member nil *))) base-name sort))
  (let ((base-sort (info-base-sort *info*)))
    (assert (not (ordered:find base-sort base-name)))
    (ordered:record base-sort (cons base-name sort))
    nil))

(defvar *sort-solution*)
(declaim (type solution:solution *sort-solution*))

(defun find-sort (flow)
  (check-type flow (cons symbol))
  (assert (every #'symbolp flow))
  (destructuring-bind (var . vars) flow
    (let* ((solution *sort-solution*)
           (sort (solution:sort-or-null solution var)))
      (check-type sort (and symbol (not (member nil *))))
      (assert (every (lambda (var)
                       (eql sort (solution:sort-or-null solution var)))
                     vars))
      sort)))

(defgeneric %populate-flow-info (skeleton))

(defmethod %populate-flow-info ((skeleton s:function))
  (destructuring-bind (arguments results)
      (s:split skeleton)
    (map nil #'%populate-flow-info arguments)
    (map nil #'%populate-flow-info results)))

(defmethod %populate-flow-info ((skeleton s:box))
  (destructuring-bind (contents)
      (s:split skeleton)
    (%populate-flow-info contents)))

(defmethod %populate-flow-info ((skeleton s:base))
  ;; keep note of every s:base we've seen.
  (ordered:record (info-bases *info*) skeleton)
  (destructuring-bind (name polarity flow)
      (s:split skeleton)
    (register-sort name (find-sort flow))
    (let ((destination (ecase polarity
                         (- (info-flow-sources *info*))
                         (+ (info-flow-sinks *info*)))))
      (flet ((register-flow (var)
               (let ((set (or (ordered:find destination var)
                              (let ((set (ordered:set :test #'equalp)))
                                (ordered:record destination (cons var set))
                                set))))
                 (ordered:record set skeleton))))
        (map nil #'register-flow flow)))))

(defun info (skeleton *sort-solution*)
  (declare (type s:function skeleton)
           (type solution:solution *sort-solution*))
  (let ((*info* (make-info)))
    (%populate-flow-info skeleton)
    *info*))
