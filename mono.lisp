(defpackage "MONO-TYPE"
  (:use "CL")
  (:local-nicknames ("C" "CONDITION"))
  (:shadow "TYPE" "FUNCTION")
  (:export "TYPE" "FUNCTION" "BASE" "BOX"
           "TYPE-P" "FUNCTION-P" "BASE-P" "BOX-P"
           "SPLIT" "PARSE"))

(in-package "MONO-TYPE")

(defgeneric split (value)
  (:documentation "Returns the constituent fields of the base type as a list."))

(defgeneric build (tag arguments)
  (:documentation "Inverse of split."))

(defun parse (expression)
  (if (typep expression '(cons symbol))
      (destructuring-bind (tag &rest args) expression
        (build tag args))
      (mapcar #'parse expression)))

(defstruct (type
             (:constructor nil)))

(defun list-of-mono (x)
  (and (listp x)
       (every #'type-p x)))

(defstruct (function
             (:include type)
             (:constructor function (arguments results)))
  (arguments nil
             :type (and list (satisfies list-of-mono))
             :read-only t)
  (results nil
           :type (and list (satisfies list-of-mono))
           :read-only t))

(defmethod split ((mono function))
  (list (function-arguments mono)
        (function-results mono)))

(defmethod build ((tag (eql 'function)) arguments)
  (destructuring-bind (arguments results) arguments
    (function (parse arguments)
              (parse results))))

(defstruct (base
             (:include type)
             (:constructor base (sort condition)))
  (sort nil :type (and symbol (not (member nil *))) :read-only t)
  (condition 'c:true :type (or list symbol) :read-only t))

(defmethod split ((mono base))
  (list (base-sort mono) (base-condition mono)))

(defmethod build ((tag (eql 'base)) arguments)
  (apply #'base arguments))

(defstruct (box
             (:include type)
             (:constructor box (contents)))
  (contents nil :type type :read-only t))

(defmethod split ((mono box))
  (list (box-contents mono)))

(defmethod build ((tag (eql 'box)) arguments)
  (destructuring-bind (contents) arguments
    (box (parse contents))))
