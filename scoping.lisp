(defpackage "SCOPING"
  (:use "CL")
  (:export "WITH-ENVIRONMENT"
           "WITH-FUNCTION-SCOPE" "WITH-FUNCTION-RETURN-VALUES"
           "LOOKUP" "REFERENCE" "TO-GLOBAL" "TO-LOCAL")
  (:local-nicknames ("C" "CONDITION")))

(in-package "SCOPING")

(defstruct scope
  (arguments nil :type simple-vector :read-only t)
  (results nil :type (or null simple-vector)))

;; vector of scopes
(defvar *environment*)
(declaim (type vector *environment*))

;; alist from key to absolute position.
(defvar *reverse-mapping*)
(declaim (type list *reverse-mapping*))

(defun %call-with-environment (thunk)
  (let ((*environment* (make-array 8 :adjustable t :fill-pointer 0))
        (*reverse-mapping* '()))
    (funcall thunk)))

(defmacro with-environment (() &body body)
  `(%call-with-environment (lambda () ,@body)))

(defun %call-with-function-scope (arguments thunk)
  (let* ((arguments (coerce arguments 'simple-vector))
         (scope (make-scope :arguments arguments))
         (environment *environment*)
         (depth (length environment))
         (*reverse-mapping* *reverse-mapping*))
    (unwind-protect
         (progn
           (vector-push-extend scope environment)
           (loop
              for arg across arguments
              for i upfrom 0
              do (push (cons arg `(c:@- ,i ,depth)) *reverse-mapping*))
           (funcall thunk))
      (when (and (plusp (length environment))
                 (eql (aref environment (1- (length environment)))
                      scope))
        (vector-pop environment)))))

(defmacro with-function-scope ((arguments) &body body)
  `(%call-with-function-scope ,arguments (lambda () ,@body)))

(defun %call-with-function-return-values (return-values thunk)
  (let ((environment *environment*))
    (assert (plusp (length environment)))
    (let* ((depth (1- (length environment)))
           (scope (aref environment depth))
           (return-values (coerce return-values 'simple-vector)))
      (assert (null (scope-results scope)))
      (setf (scope-results scope) return-values)
      (loop
         for ret across return-values
         for i upfrom 0
         do (push (cons ret `(c:@+ ,i ,depth)) *reverse-mapping*))
      (funcall thunk))))

(defmacro with-function-return-values ((return-values) &body body)
  `(%call-with-function-return-values ,return-values (lambda () ,@body)))

(defun lookup (operator index depth fail-tag)
  (let ((environment *environment*))
    (when (>= depth (length environment))
      (throw fail-tag "Depth out of bounds"))
    (let* ((scope (aref environment (- (length environment) 1 depth)))
           (vector (ecase operator
                     (c:@- (scope-arguments scope))
                     (c:@+ (let ((results (scope-results scope)))
                             (unless results
                               (throw fail-tag "Arguments not yet in scope"))
                             results))))
           (index (if (minusp index)
                      (+ (length vector) index)
                      index)))
      (when (>= index (length vector))
        (throw fail-tag "Index out of bounds"))
      (aref vector index))))

(defun reference (object &key (test 'eql) (key 'identity))
  (let ((entry (cdr (assoc object *reverse-mapping*
                           :test test
                           :key key))))
    (and entry
         (destructuring-bind (operator index depth) entry
           (let ((relative-depth (- (length *environment*) 1 depth)))
             (assert (>= relative-depth 0))
             (if (zerop relative-depth)
                 (list operator index)
                 (list operator index relative-depth)))))))

;; If flip-toplevel is true, references to arguments values at the
;; topmost scope actually refer to return values, and references to
;; toplevel return values are out of bounds.
(defun to-global (condition fail-tag &key flip-toplevel)
  (labels ((walk (condition)
             (etypecase condition
               ((cons (member c:@+ c:@-))
                (destructuring-bind (operator index &optional (depth 0))
                    condition
                  (when (and flip-toplevel
                             (= depth (1- (length *environment*))))
                    (ecase operator
                      (c:@- (setf operator 'c:@+))
                      (c:@+ (throw fail-tag
                              (format nil "Return value pattern has ~S; must ~
                                           not refer to toplevel return values."
                                      condition)))))
                  (lookup operator index depth fail-tag)))
               (list
                (mapcar #'walk condition))
               (t
                condition))))
    (walk condition)))

(defun to-local (condition replacement-predicate replacement-function
                 &key (test 'eql) (key 'identity))
  (labels ((walk (condition)
             (cond ((funcall replacement-predicate condition)
                    (funcall replacement-function
                             condition (reference condition :test test :key key)))
                   ((listp condition)
                    (mapcar #'walk condition))
                   (t
                    condition))))
    (walk condition)))
