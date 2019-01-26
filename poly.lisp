(defpackage "POLY-TYPE"
  (:use "CL")
  (:local-nicknames ("C" "CONDITION"))
  (:shadow "TYPE" "FUNCTION")
  (:export "*"
           "TYPE" "FUNCTION" "BOX" "BASE" "VAR"
           "TYPE-P" "FUNCTION-P" "BOX-P" "BASE-P" "VAR-P"
           "SPLIT" "PARSE"))

(in-package "POLY-TYPE")

(defgeneric split (value))

(defgeneric build (tag arguments))

(defun parse (expression)
  (if (typep expression '(cons symbol))
      (destructuring-bind (tag &rest args) expression
        (build tag args))
      (mapcar #'parse expression)))

(defstruct (type
             (:constructor nil)))

(defun list-of-pattern (x)
  (and (listp x)
       (every #'type-p x)))

(defun list-of-symbol (x)
  (and (listp x)
       (every #'symbolp x)))

(defstruct (function
             (:include type)
             (:constructor function (arguments results)))
  (arguments nil
             :type (and list (satisfies list-of-pattern))
             :read-only t)
  (results nil
           :type (and list (satisfies list-of-pattern))
           :read-only t))

(defmethod split ((pattern function))
  (list (function-arguments pattern)
        (function-results pattern)))

(defmethod build ((tag (eql 'function)) arguments)
  (destructuring-bind (arguments results) arguments
    (function (parse arguments)
              (parse results))))

(defstruct (box
             (:include type)
             (:constructor box (contents)))
  (contents nil :type type :read-only t))

(defmethod split ((pattern box))
  (list (box-contents pattern)))

(defmethod build ((tag (eql 'box)) arguments)
  (destructuring-bind (contents) arguments
    (box (parse contents))))

(defun %ensure-flow-empty-iff-sort-specified (flow sort)
  (if (eql sort '*)
      (assert flow)
      (assert (null flow)))
  flow)

(defstruct (base
             (:include type)
             (:constructor base (flow sort condition)))
  ;; list of equivalence classes where this type flows into and out
  ;; of, depending on polarity.
  ;;
  ;; this list must not be empty. If a fully specified type doesn't
  ;; need to participate in the dataflow, it makes sense to specify a
  ;; unique flow variable.
  (flow nil :type (and cons (satisfies list-of-symbol)) :read-only t)
  ;; * is a special symbol that stands for nothing in
  (sort '* :type symbol :read-only t)
  ;; nil means no additional condition
  (condition nil :type (or null symbol list) :read-only t))

(defmethod split ((pattern base))
  (list (base-flow pattern)
        (base-sort pattern)
        (base-condition pattern)))

(defmethod build ((tag (eql 'base)) arguments)
  (apply #'base arguments))

(defstruct (var
             (:include type)
             (:constructor var (flow)))
  (flow nil :type (and cons (satisfies list-of-symbol)) :read-only t))

(defmethod split ((pattern var))
  (list (var-flow pattern)))

(defmethod build ((tag (eql 'var)) arguments)
  (apply #'var arguments))
