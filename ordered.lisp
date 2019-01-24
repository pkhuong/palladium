;;; Set of values with a stable iteration order.
(defpackage "ORDERED"
  (:use "CL")
  (:shadow "SET" "MAP" "FIND")
  (:export "SET" "MAP" "RECORD" "RECORD-ALL" "ENTRIES" "FIND" "CONTAINS"))

(in-package "ORDERED")

(defstruct (base-set
             (:constructor nil))
  (seen nil :type hash-table :read-only t)
  (values nil :type list))

(defstruct (set
             (:include base-set)))

(defun set (&key (test 'eql))
  (make-set :seen (make-hash-table :test test)))

(defstruct (map
             (:include base-set)))

(defun map (&key (test 'eql))
  (make-map :seen (make-hash-table :test test)))

(defun record (sink &rest values)
  (record-all sink values))

(defun record-all (sink values)
  (cl:map nil (lambda (value)
                (record-one-value sink value))
          values)
  sink)

(defun find (sink key)
  (gethash key (base-set-seen sink)))

(defun contains (sink key)
  (nth-value 1 (find sink key)))

(defgeneric entries (sink &key sort))

(defmethod entries ((sink set) &key sort)
  (let ((values (reverse (set-values sink))))
    (if sort
        (stable-sort values sort)
        values)))

(defmethod entries ((sink map) &key sort)
  (let ((seen (map-seen sink))
        (values (reverse (map-values sink))))
    (mapcar (lambda (key)
              (cons key (gethash key seen)))
            (if sort
                (stable-sort values sort)
                values))))

(defgeneric record-one-value (sink value))

(defmethod record-one-value ((sink set) value)
  (unless (contains sink value)
    (setf (gethash value (set-seen sink)) t)
    (push value (set-values sink)))
  sink)

(defmethod record-one-value ((sink map) pair)
  (destructuring-bind (key . value) pair
    (unless (contains sink key)
      (push key (map-values sink)))
    (setf (gethash key (map-seen sink)) value)
    sink))

