;;; An attributed disjoint set data structure maintains equivalence
;;; classes for a set of keys, as well as a map from equivalence class
;;; to attribute.
;;;
;;; Concretely, it is a pair of a union-find data structure that also
;;; maps each class's representative to its attribute, if any.
(defpackage "ATTRIBUTED-DISJOINT-SET"
  (:use "CL")
  (:shadow "MAP" "FIND")
  (:import-from "COERCE-TO-FUN" "COERCE-TO-FUN")
  (:export "MAP" "OBSERVE-EQUIVALENCE"
           "FIND" "FOREACH-ELEMENT" "DO-ELEMENTS"))

(in-package "ATTRIBUTED-DISJOINT-SET")

;;; Basic types
(defstruct union-find-entry
  (parent nil :type (or null union-find-entry))
  (attribute nil :type t)
  (rank 0 :type (unsigned-byte 32))
  ;; only valid if parent is NIL, i.e., the entry is its class
  ;; representative.
  (has-attribute 0 :type bit))

(defstruct map
  ;; key -> union-find-entry
  (entries nil :type ordered:map :read-only t)
  (merge-function nil :type function :read-only t))

(defun map (attribute-merge-function &key (test 'eql))
  (make-map :entries (ordered:map :test test)
            :merge-function (coerce-to-fun attribute-merge-function)))

(defun find-representative (entry)
  (let ((previous entry) ;; after the first iteration,
        (current entry)) ;; (union-find-entry previous) -> current
    (loop
       for parent = (union-find-entry-parent current)
       while parent
       do (setf (union-find-entry-parent previous) parent)
          (shiftf previous current parent))
    ;; cache the result in case of repeated lookups.
    (unless (eql entry current)
      (setf (union-find-entry-parent entry) current))
    current))

(defun merge-attribute (entry new-attribute merge-function)
  (declare (type union-find-entry entry)
           (type function merge-function))
  ;; we only attach attributes to parents
  (assert (null (union-find-entry-parent entry)))
  (cond ((zerop (union-find-entry-has-attribute entry))
            (setf (union-find-entry-has-attribute entry) 1
                  (union-find-entry-attribute entry) new-attribute))
           (t
            (setf (union-find-entry-attribute entry)
                  (funcall merge-function (union-find-entry-attribute entry)
                           new-attribute)))))

;;; Grow equivalence classes
(defun ensure-entry (map key)
  (declare (type map map))
  (let ((entries (map-entries map)))
    (or (ordered:find entries key)
        (let ((entry (make-union-find-entry)))
          (ordered:record entries (cons key entry))
          entry))))

(defun merge-representatives (x y merge-function)
  (declare (type union-find-entry x y)
           (type function merge-function))
  ;; We should have two roots.
  (assert (not (union-find-entry-parent x)))
  (assert (not (union-find-entry-parent y)))
  (when (eql x y)
    (return-from merge-representatives x))
  ;; (union-find-entry-rank x) >= (union-find-entry-rank y)
  (unless (>= (union-find-entry-rank x) (union-find-entry-rank y))
    (rotatef x y))
  ;; Merge attributes into x, and clear y's.
  (when (plusp (union-find-entry-has-attribute y))
    (merge-attribute x (shiftf (union-find-entry-attribute y) nil)
                     merge-function)
    (setf (union-find-entry-has-attribute y) 0))
  (assert (zerop (union-find-entry-has-attribute y)))
  (assert (null (union-find-entry-attribute y)))
  ;; Make x the parent of y, and increase x's rank if necessary.
  (setf (union-find-entry-parent y) x)
  (setf (union-find-entry-rank x) (max (union-find-entry-rank x)
                                       (1+ (union-find-entry-rank y))))
  x)

;;; Public modification function.
(defun %observe-equivalence (map first-key keys)
  (declare (type map map))
  (let ((accumulator (find-representative (ensure-entry map first-key)))
        (merge-function (map-merge-function map)))
    (dolist (key keys accumulator)
      (setf accumulator
            (merge-representatives accumulator
                                   (find-representative
                                    (ensure-entry map key))
                                   merge-function)))))

(defun observe-equivalence (map keys &key (attribute nil attributep))
  (when keys
    (let ((representative (%observe-equivalence map (first keys) (rest keys))))
      (when attributep
        (merge-attribute representative attribute (map-merge-function map)))))
  map)

(defun find (map key)
  (let ((entry (ordered:find (map-entries map) key)))
    (unless entry
      (return-from find (values nil nil)))
    (let ((root (find-representative entry)))
      (if (plusp (union-find-entry-has-attribute root))
          (values (union-find-entry-attribute root) t)
          (values nil nil)))))

;;; Iteration function / macro
(defun %call-with-each-element (map function sort)
  (declare (type map map)
           (type function function)
           (type (or null t #|callable|#) sort))
  (loop
     for (key . entry) in (ordered:entries (map-entries map) :sort sort)
     for root = (find-representative entry)
     for has-value = (plusp (union-find-entry-has-attribute root))
     for value = (and has-value
                      (union-find-entry-attribute root))
     do (funcall function key value has-value)))

(defun foreach-element (map function &key sort)
  (%call-with-each-element map
                           (coerce-to-fun function)
                           (and sort
                                (coerce-to-fun sort))))

(defmacro do-elements (((key value has-value) map return-value &key sort)
                       &body body)
  `(block nil
     (foreach-element ,map
                      (lambda (,key ,value ,has-value)
                        ,@body)
                      :sort ,sort)
     ,return-value))
