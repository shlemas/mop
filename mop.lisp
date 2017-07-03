; Copyright (c) 2017, Shawn LeMaster
;
; Permission to use, copy, modify, and/or distribute this software for any
; purpose with or without fee is hereby granted, provided that the above
; copyright notice and this permission notice appear in all copies.
;
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
; REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
; INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
; OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
; PERFORMANCE OF THIS SOFTWARE.

;-------------------------------------------------------------------------------
; Utility
;-------------------------------------------------------------------------------

(defmacro mop-push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

(defun (setf getf*) (new-value plist key)
  (block body
    (do ((x plist (cddr x)))
        ((null x))
      (when (eq (car x) key)
        (setf (car (cdr x)) new-value)
        (return-from body new-value)))
    (mop-push-on-end key plist)
    (mop-push-on-end new-value plist)
    new-value))

(defun mop-mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mop-mapappend fun (mapcar #'cdr args)))))

(defun mop-mapplist (fun x)
  (if (null x)
      ()
      (cons (funcall fun (car x) (cadr x))
            (mop-mapplist fun (cddr x)))))

(defstruct (mop-std-instance (:constructor mop-allocate-std-instance (class slots))
                             (:predicate mop-std-instance-p)
                             (:print-function mop-print-std-instance))
  class
  slots)

(defun mop-print-std-instance (instance stream depth)
  (declare (ignore depth))
  (mop-print-object instance stream))

(defparameter mop-secret-unbound-value (list "slot unbound)"))

(defun mop-instance-slot-p (slot)
  (eq (mop-slot-definition-allocation slot) ':instance))

(defun mop-std-allocate-instance (class)
  (mop-allocate-std-instance class
                             (mop-allocate-slot-storage (count-if #'mop-instance-slot-p (mop-class-slots class))
                                                        mop-secret-unbound-value)))

(defun mop-allocate-slot-storage (size initial-value)
  (make-array size :initial-element initial-value))

(defvar mop-the-slots-of-standard-class)
(defvar mop-the-class-standard-class)

(defun mop-slot-location (class slot-name)
  (if (and (eq slot-name 'effective-slots)
           (eq class mop-std-the-class-standard-class))
      (position 'effective-slots mop-the-slots-of-standard-class :key #'mop-slot-definition-name)
      (let ((slot (find slot-name (mop-class-slots class) :key #'mop-slot-definition-name)))
        (if (null slot)
            (error "The slot ~S is missing from the class ~S." slot-name class)
            (let ((pos (position slot (remove-if-not #'mop-instance-slot-p (mop-class-slots class)))))
              (if (null pos)
                  (error "The slot ~S is not an instance slot in the class ~S." slot-name class)
                  pos))))))

(defun mop-slot-contents (slots location)
  (svref slots location))

(defun (setf mop-slot-contents) (new-value slots location)
  (setf (svref slots location) new-value))

(defun mop-std-slot-value (instance slot-name)
  (let* ((location (mop-slot-location (mop-class-of instance) slot-name))
         (slots (mop-std-instance-slots instance))
         (val (mop-slot-contents slots location)))
    (if (eq mop-secret-unbound-value val)
        (error "The slot ~S is unbound in the object ~S." slot-name instance)
        val)))

(defun mop-slot-value (object slot-name)
  (if (eq (mop-class-of (mop-class-of object)) mop-the-class-standard-class)
      (mop-std-slot-value object slot-name)
      (mop-slot-value-using-class (mop-class-of object) object slot-name)))

(defun (setf mop-std-slot-value) (new-value instance slot-name)
  (let ((location (mop-slot-location (mop-class-of instance) slot-name))
        (slots (mop-std-instance-slots instance)))
    (setf (mop-slot-contents slots location) new-value)))

(defun (setf mop-slot-value) (new-value object slot-name)
  (if (eq (mop-class-of (mop-class-of object)) mop-the-class-standard-class)
      (setf (mop-std-slot-value object slot-name) new-value)
      (mop-setf-slot-value-using-class new-value (mop-class-of object) object slot-name)))

(defun mop-std-slot-boundp (instance slot-name)
  (let ((location (mop-std-location (mop-class-of instance) slot-name))
        (slots (mop-std-instance-slots instance)))
    (not (eq mop-secret-unbound-value (mop-slot-contents slots location)))))

(defun mop-slot-boundp (object slot-name)
  (if (eq (mop-class-of (mop-class-of object)) mop-the-class-standard-class)
      (mop-std-slot-boundp object slot-name)
      (mop-slot-boundp-using-class (mop-class-of object) object slot-name)))

(defun mop-std-slot-makunbound (instance slot-name)
  (let ((location (mop-slot-location (mop-class-of instance) slot-name))
        (slots (mop-std-instance-slots instance)))
    (setf (mop-slot-contents slots location) mop-secret-unbound-value))
  instance)

(defun mop-slot-makunbound (object slot-name)
  (if (eq (mop-class-of (mop-class-of object)) mop-the-class-standard-class)
      (mop-std-slot-makunbound object slot-name)
      (mop-slot-makunbound-using-class (mop-class-of object) object slot-name)))

(defun mop-std-slot-exists-p (instance slot-name)
  (not (null (find slot-name (mop-class-slots (mop-class-of instance))
                   :key #'mop-slot-definition-name))))

(defun mop-slot-exists-p (object slot-name)
  (if (eq (mop-class-of (mop-class-of object)) mop-the-class-standard-class)
      (mop-std-slot-exists-p object slot-name)
      (mop-slot-exists-p-using-class (mop-class-of object) object slot-name)))

(defun mop-class-of (x)
  (if (mop-std-instance-p x)
      (mop-std-instance-class x)
      (mop-built-in-class-of x)))

(defun mop-built-in-class-of (x)
  (typecase x
    (null (mop-find-class 'null))
    ((and symbol (not null)) (mop-find-class 'symbol))
    ((complex *) (mop-find-class 'complex))
    ((integer * *) (mop-find-class 'integer))
    ((float * *) (mop-find-class 'float))
    (cons (mop-find-class 'cons))
    (character (mop-find-class 'character))
    (hash-table (mop-find-class 'hash-table))
    (package (mop-find-class 'package))
    (pathname (mop-find-class 'pathname))
    (readtable (mop-find-class 'readtable))
    (stream (mop-find-class 'stream))
    ((and number (not (or integer complex float))) (mop-find-class 'number))
    ((string *) (mop-find-class 'string))
    ((bit-vector *) (mop-find-class 'bit-vector))
    ((and (vector * *) (not (or string vector))) (mop-find-class 'vector))
    ((and (array * *) (not vector)) (mop-find-class 'array))
    ((and sequence (not (or vector list))) (mop-find-class 'sequence))
    (function (mop-find-class 'function))
    (t (mop-find-class 't))))

(defun mop-subclassp (c1 c2)
  (not (null (find c2 (mop-class-precedence-list c1)))))

(defun mop-sub-specializer-p (c1 c2 c-arg)
  (let ((cpl (mop-class-precedence-list c-arg)))
    (not (null (find c2 (cdr (member c1 cpl)))))))

(defparameter mop-the-defclass-standard-class
  '(mop-defclass mop-standard-class ()
     ((name :initarg :name)
      (direct-superclasses :initarg :direct-superclasses)
      (direct-slots)
      (class-precedence-list)
      (effective-slots)
      (direct-subclasses :initform ())
      (direct-methods :initform ()))))

(defun mop-class-name (class)
  (mop-std-slot-value class 'name))
(defun (setf mop-class-name) (new-value class)
  (setf (mop-slot-value class 'name) new-value))

(defun mop-class-direct-superclasses (class)
  (mop-slot-value class 'direct-superclasses))
(defun (setf mop-class-direct-superclasses) (new-value class)
  (setf (mop-slot-value class 'direct-superclasses) new-value))

(defun mop-class-direct-slots (class)
  (mop-slot-value class 'direct-slots))
(defun (setf mop-class-direct-slots) (new-value class)
  (setf (mop-slot-value class 'direct-slots) new-value))

(defun mop-class-precedence-list (class)
  (mop-slot-value class 'class-precedence-list))
(defun (setf mop-class-precedence-list) (new-value class)
  (setf (mop-slot-value class 'class-precedence-list) new-value))

(defun mop-class-slots (class)
  (mop-slot-value class 'effective-slots))
(defun (setf mop-class-slots) (new-value class)
  (setf (mop-slot-value class 'effective-slots) new-value))

(defun mop-direct-subclasses (class)
  (mop-slot-value class 'direct-subclasses))
(defun (setf mop-class-direct-subclasses) (new-value class)
  (setf (mop-slot-value class 'direct-subclasses) new-value))

(defun mop-direct-methods (class)
  (mop-slot-value class 'direct-methods))
(defun (setf mop-class-direct-methods) (new-value class)
  (setf (mop-slot-value class 'direct-methods) new-value))

(defmacro mop-defclass (name direct-superclasses direct-slots &rest options)
  `(mop-ensure-class ',name
     :direct-superclasses ,(mop-canonicalize-direct-superclasses direct-superclasses)
     :direct-slots ,(mop-canonicalize-direct-slots direct-slots)
     ,@(mop-canonicalize-defclass-options options)))

(defun mop-canonicalize-direct-slots (direct-slots)
  `(list ,@(mapcar #'mop-canonicalize-direct-slot direct-slots)))

(defun mop-canonicalize-direct-slot (spec)
  (if (symbolp spec)
      `(list :name ',spec)
      (let ((name (car spec))
            (initfunction nil)
            (initform nil)
            (initargs ())
            (readers ())
            (writers ())
            (other-options ()))
        (do ((olist (cdr spec) (cddr olist)))
            ((null olist))
          (case (car olist)
            (:initform (setq initfunction `(function (lambda () ,(cadr olist))))
                       (setq initform `',(cadr olist)))
            (:initarg (mop-push-on-end (cadr olist) initargs))
            (:reader (mop-push-on-end (cadr olist) readers))
            (:writer (mop-push-on-end (cadr olist) writers))
            (:accessor (mop-push-on-end (cadr olist) readers)
                       (mop-push-on-end `(setf ,(cadr olist)) writers))
            (otherwise (mop-push-on-end `',(car olist) other-options)
                       (mop-push-on-end `',(cadr olist) other-options))))
        `(list :name ',name
               ,@(when initfunction `(:initform ,initform :initfunction ,initfunction))
               ,@(when initargs `(:initargs ',initargs))
               ,@(when readers `(:readers ',readers))
               ,@(when writers `(:writers ',writers))
               ,@other-options))))

(defun mop-canonicalize-direct-superclasses (direct-superclasses)
  `(list ,@(mapcar #'mop-canonicalize-direct-superclass direct-superclasses)))

(defun mop-canonicalize-direct-superclass (class-name)
  `(find-class ',class-name))

(defun mop-canonicalize-defclass-option (option)
  (case (car option)
    (:metaclass (list ':metaclass `(find-class ',(cadr option))))
    (:default-initargs (list ':direct-default-initargs
                             `(list ,@(mop-mapappend #'(lambda (x) x)
                                                  (mop-mapplist #'(lambda (key value)
                                                                    `(',key ,value))
                                                                (cdr option))))))
    (otherwise (list `',(car option) `',(cadr option)))))

(defun mop-canonicalize-defclass-options (options)
  (mop-mapappend #'mop-canonicalize-defclass-option options))

; Stub
(defun mop-make-instance (class-name &rest all-keys)
  (list class-name all-keys))

(let ((mop-class-table (make-hash-table :test #'eq)))

  (defun mop-find-class (symbol &optional (errorp t))
    (let ((class (gethash symbol mop-class-table nil)))
      (if (and (null class) errorp)
          (error "No class named ~S." symbol)
          class)))

  (defun (setf mop-find-class) (new-value symbol)
    (setf (gethash symbol mop-class-table) new-value))

  (defun mop-forget-all-classes ()
    (clrhash mop-class-table)
    (values))

)

(defun mop-ensure-class (name &rest all-keys)
  (if (mop-find-class name nil)
      (error "Can't redefine the class named ~S." name)
      (let ((class (apply #'mop-make-instance 'mop-standard-class :name name all-keys)))
        (setf (mop-find-class name) class)
        class)))

(defun mop-make-instance-standard-class (metaclass
                                         &key name direct-superclasses direct-slots
                                         &allow-other-keys)
  (declare (ignore metaclass))
  (let ((class (mop-std-allocate-instance mop-the-class-standard-class)))
    (setf (mop-class-name class) name)
    (setf (mop-class-direct-subclasses class) ())
    (setf (mop-class-direct-methods class) ())
    (mop-std-after-initialization-for-classes class
                                              :direct-slots direct-slots
                                              :direct-superclasses direct-superclassess)
    class))

(defun mop-std-after-initialization-for-classes (class
                                                 &key direct-superclasses direct-slots
                                                 &allow-other-keys)
  (let ((supers (or direct-superclasses (list (mop-find-class 'mop-standard-object)))))
    (setf (mop-class-direct-superclasses class) supers)
    (dolist (superclass supers)
      (push class (mop-class-direct-subclasses superclass))))
  (let ((slots (mapcar #'(lambda (slot-properties)
                           (apply #'mop-make-direct-slot-definition slot-properties))
                           direct-slots)))
    (setf (mop-class-direct-slots class) slots)
    (dolist (direct-slot slots)
      (dolist (reader (mop-slot-definition-readers direct-slot))
        (mop-add-reader-method class reader (mop-slot-definition-name direct-slot)))
      (dolist (writer (mop-slot-definition-writers direct-slot))
        (mop-add-writer-method class writer (mop-slot-definition-name direct-slot)))))
  (funcall (if (eq (mop-class-of class) mop-the-standard-class)
               #'mop-std-finalize-inheritance
               #'mop-finalize-inheritance)
           class)
  (values))

(defun mop-make-direct-slot-definition (&rest properties
                                        &key name
                                             (initargs ())
                                             (initform nil)
                                             (initfunction nil)
                                             (readers ())
                                             (writers ())
                                             (allocation :instance)
                                        &allow-other-keys)
  (let ((slot (copy-list properties)))
    (setf (getf* slot ':name) name)
    (setf (getf* slot ':initargs) initargs)
    (setf (getf* slot ':initform) initform)
    (setf (getf* slot ':initfunction) initfunction)
    (setf (getf* slot ':readers) readers)
    (setf (getf* slot ':writers) writers)
    (setf (getf* slot ':allocation) allocation)
    slot))

(defun mop-make-effective-slot-definition (&rest properties
                                           &key name
                                                (initargs ())
                                                (initform nil)
                                                (initfunction nil)
                                                (allocation :instance)
                                           &allow-other-keys)
  (let ((slot (copy-list properties)))
    (setf (getf* slot ':name) name)
    (setf (getf* slot ':initargs) initargs)
    (setf (getf* slot ':initform) initform)
    (setf (getf* slot ':initfunction) initfunction)
    (setf (getf* slot ':allocation) allocation)
    slot))

(defun mop-slot-definition-name (slot)
  (getf slot ':name))
(defun (setf mop-slot-definition-name) (new-value slot)
  (setf (getf* slot ':name) new-value))

(defun mop-slot-definition-initfunction (slot)
  (getf slot ':initfunction))
(defun (setf mop-slot-definition-initfunction) (new-value slot)
  (setf (getf* slot ':initfunction) new-value))

(defun mop-slot-definition-initform (slot)
  (getf slot ':initform))
(defun (setf mop-slot-definition-initform) (new-value slot)
  (setf (getf* slot ':initform) new-value))

(defun mop-slot-definition-initargs (slot)
  (getf slot ':initargs))
(defun (setf mop-slot-definition-initargs) (new-value slot)
  (setf (getf* slot ':initargs) new-value))

(defun mop-slot-definition-readers (slot)
  (getf slot ':readers))
(defun (setf mop-slot-definition-readers) (new-value slot)
  (setf (getf* slot ':readers) new-value))

(defun mop-slot-definition-writers (slot)
  (getf slot ':writers))
(defun (setf mop-slot-definition-writers) (new-value slot)
  (setf (getf* slot ':writers) new-value))

(defun mop-slot-definition-allocation (slot)
  (getf slot ':allocation))
(defun (setf mop-slot-definition-allocation) (new-value slot)
  (setf (getf* slot ':allocation) new-value))

(defun mop-std-finalize-inheritance (class)
  (setf (mop-class-precedence-list class)
        (funcall (if (eq (mop-class-of class) mop-the-class-standard-class)
                     #'mop-std-compute-class-precedence-list
                     #'mop-compute-class-precedence-list)
                 class))
  (setf (mop-class-slots class)
        (funcall (if (eq (mop-class-of class) mop-the-class-standard-class)
                     #'mop-std-compute-slots
                     #'mop-compute-slots)
                 class))
  (values))

(defun mop-std-compute-class-precedence-list (class)
  (let ((classes-to-order (mop-collect-superclasses* class)))
    (mop-topological-sort classes-to-order
                          (remove-duplicates
                           (mop-mapappand #'mop-local-precedence-order classes-to-order))
                          #'mop-std-tie-breaker-rule)))

(defun mop-topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ()))
    (loop (let ((minimal-elements (remove-if #'(lambda (class)
                                                 (member class remaining-constraints
                                                         :key #'cadr))
                                             remaining-elements)))
            (when (null minimal-elements)
              (if (null remaining-elements)
                  (return-from mop-topological-sort result)
                  (error "Inconsistent precedence graph.")))
            (let ((choice (if (null (cdr minimal-elements))
                              (car minimal-elements)
                              (funcall tie-breaker minimal-elements result))))
              (setq result (append result (list choice)))
              (setq remaining-elements (remove choice remaining-elements))
              (setq remaining-constraints (remove choice remaining-constraints :test #'member)))))))

(defun mop-std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (mop-class-direct-superclasses cpl-constituent))
           (common (intersection minimal-elements cpl-constituent)))
      (when (not (null common))
        (return-from mop-std-tie-breaker-rule (car common))))))

(defun mop-collect-superclasses* (class)
  (labels ((all-superclasses-loop (seen superclasses)
             (let ((to-be-processed (set-difference superclasses seen)))
               (if (null to-be-processed)
                   superclasses
                   (let ((class-to-process (car to-be-processed)))
                     (all-superclasses-loop
                      (cons class-to-process seen)
                      (union (mop-class-direct-superclasses class-to-process)
                             superclasses)))))))
    (all-superclasses-loop () (list class))))

(defun mop-local-precedence-order (class)
  (mapcar #'list
          (cons class (butlast (mop-class-direct-superclasses class)))
          (mop-class-direct-superclasses class)))

(defun mop-std-compute-slots (class)
  (let* ((all-slots (mop-mapappend #'mop-class-direct-slots (mop-class-precedence-list class)))
         (all-names (remove-duplicates (mapcar #'mop-slot-definition-name all-slots))))
    (mapcar #'(lambda (name)
                (funcall (if (eq (mop-class-of class) mop-the-class-standard-class)
                             #'mop-std-compute-effective-slot-definition
                             #'std-compute-effective-slot-definition)
                         class
                         (remove name all-slots :key #'mop-slot-definition-name :test-not #'eq)))
            all-names)))

(defun mop-std-compute-effective-slot-definition (class direct-slots)
  (declare (ignore class))
  (let ((initer (find-if-not #'null direct-slots :key #'mop-slot-definition-initfunction)))
    (mop-make-effective-slot-definition
     :name (mop-slot-definition-name (car direct-slots))
     :initform (if initer (mop-slot-definition-iniform initer) nil)
     :initfunction (if initer (mop-slot-definition-initfunction initer) nil)
     :initargs (remove-duplicates (mop-mapappend #'mop-slot-definition-initargs direct-slots))
     :allocation (mop-slot-definition-allocation (car direct-slots)))))

(defparameter mop-the-defclass-standard-generic-function
  `(defclass mop-standard-generic-function ()
     ((name :initarg :name)
      (lambda-list :initarg :lambda-list)
      (methods :initform ())
      (method-class :initarg :method-class)
      (discriminating-function)
      (classes-to-emf-table :initform (make-hash-table :test #'equal)))))

(defvar mop-the-class-standard-gf)

(defun mop-generic-function-name (gf)
  (mop-slot-value gf 'name))
(defun (setf mop-generic-function-name) (new-value gf)
  (setf (mop-slot-value gf 'name) new-value))

(defun mop-generic-function-lambda-list (gf)
  (mop-slot-value gf 'lambda-list))
(defun (setf mop-generic-function-lambda-list) (new-value gf)
  (setf (mop-slot-value gf 'lambda-list) new-value))

(defun mop-generic-function-methods (gf)
  (mop-slot-value gf 'methods))
(defun (setf mop-generic-function-methods) (new-value gf)
  (setf (mop-slot-value gf 'methods) new-value))

(defun mop-generic-function-discriminating-function (gf)
  (mop-slot-value gf 'discriminating-function))
(defun (setf mop-generic-function-discriminating-function) (new-value gf)
  (setf (mop-slot-value gf 'discriminating-function) new-value))

(defun mop-generic-function-method-class (gf)
  (mop-slot-value gf 'method-class))
(defun (setf mop-generic-function-method-class) (new-value gf)
  (setf (mop-slot-value gf 'method-class) new-value))

(defun mop-classes-to-emf-table (gf)
  (mop-slot-value gf 'classes-to-emf-table))
(defun (setf mop-classes-to-emf-table) (new-value gf)
  (setf (mop-slot-value gf 'classes-to-emf-table) new-value))

(defparameter mop-the-defclass-standard-method
  '(defclass mop-standard-method ()
     ((lambda-list :initarg :lambda-list)
      (qualifiers :initarg :qualifiers)
      (specializers :initarg :specializers)
      (body :initarg :body)
      (environment :initarg :environment)
      (generic-function :initform nil)
      (function))))

(defvar mop-the-class-standard-method)

(defun mop-method-lambda-list (method) (mop-slot-value method 'lambda-list))
(defun (setf mop-method-lambda-list) (new-value method)
  (setf (mop-slot-value method 'lambda-list) new-value))

(defun mop-method-qualifiers (method) (mop-slot-value method 'qualifiers))
(defun (setf mop-method-qualifiers) (new-value method)
  (setf (mop-slot-value method 'qualifiers) new-value))

(defun mop-method-specializers (method) (mop-slot-value method 'specializers))
(defun (setf mop-method-specializers) (new-value method)
  (setf (mop-slot-value method 'specializers) new-value))

(defun mop-method-body (method) (mop-slot-value method 'body))
(defun (setf mop-method-body) (new-value method)
  (setf (mop-slot-value method 'body) new-value))

(defun mop-method-environment (method) (mop-slot-value method 'environment))
(defun (setf mop-method-environment) (new-value method)
  (setf (mop-slot-value method 'environment) new-value))

(defun mop-method-generic-function (method) (mop-slot-value method 'generic-function))
(defun (setf mop-method-generic-function) (new-value method)
  (setf (mop-slot-value method 'generic-function) new-value))

(defun mop-method-function (method) (mop-slot-value method 'function))
(defun (setf mop-method-function) (new-value method)
  (setf (mop-slot-value method 'function) new-value))

(defmacro mop-defgeneric (function-name lambda-list &rest options)
  `(mop-ensure-generic-function ',function-name
                                :lambda-list ',lambda-list
                                ,@(mop-canonicalize-defgeneric-options options)))

(defun mop-canonicalize-defgeneric-options (options)
  (mop-mapappend #'mop-canonicalize-defgeneric-option options))

(defun mop-canonicalize-defgeneric-option (option)
  (case (car option)
    (:generic-function-class (list ':generic-function-class `(find-class ',(cadr option))))
    (:method-class (list ':method-class `(find-class ',(cadr option))))
    (otherwise (list `',(car option) `',(cadr option)))))

(let ((mop-generic-function-table (make-hash-table :test #'equal)))

  (defun mop-find-generic-function (symbol &optional (errorp t))
    (let ((gf (gethash symbol mop-generic-function-table nil)))
      (if (and (null gf) errorp)
          (error "No generic function named ~S." symbol)
          gf)))

  (defun (setf mop-find-generic-function) (new-value symbol)
    (setf (gethash symbol mop-generic-function-table) new-value))

  (defun mop-forget-all-generic-functions ()
    (clrhash mop-generic-function-table)
    (values))

)

(defun mop-ensure-generic-function (function-name
                                    &rest all-keys
                                    &key (generic-function-class mop-the-class-standard-gf)
                                         (method-class mop-the-class-standard-method)
                                    &allow-other-keys)
  (if (mop-find-generic-function function-name nil)
      (mop-find-generic-function function-name)
      (let ((gf (apply (if (eq generic-function-class mop-the-class-standard-gf)
                           #'mop-make-instance-standard-generic-function
                           #'mop-make-instance)
                       generic-function-class
                       :name function-name
                       :method-class method-class
                       all-keys)))
        (setf (mop-find-generic-function function-name) gf)
        gf)))

(defun mop-finalize-generic-function (gf)
  (setf (mop-generic-function-discriminating-function gf)
        (funcall (if (eq (mop-class-of gf) mop-the-class-standard-gf)
                     #'mop-std-compute-discriminating-function
                     #'mop-compute-discriminating-function)
                 gf))
  (setf (fdefinition (mop-generic-function-name gf))
        (mop-generic-function-discriminating-function gf))
  (clrhash (mop-classes-to-emf-table gf))
  (values))

(defun mop-make-instance-standard-generic-function (generic-function-class
                                                    &key name lambda-list method-class)
  (declare (ignore generic-function-clsas))
  (let ((gf (mop-std-allocate-instance mop-the-class-standard-gf)))
    (setf (mop-generic-function-name gf) name)
    (setf (mop-generic-function-lambda-list gf) lambda-list)
    (setf (mop-generic-function-methods gf) ())
    (setf (mop-generic-function-method-class gf) method-class)
    (setf (mop-classes-to-emf-table gf) (make-hash-table :test #'equal))
    (mop-finalize-generic-function gf)
    gf))

(defmacro mop-defmethod (&rest args)
  (multiple-value-bind (function-name qualifiers lambda-list specializers body)
    (parse-defmethod args)
    `(ensure-method (mop-find-generic-function ',function-name)
       :lambda-list ',lambda-list
       :qualifiers ',qualifiers
       :specializers ,(mop-canonicalize-specializers specializers)
       :body ',body
       :environment (mop-top-level-environment))))

(defun mop-canonicalize-specializers (specializers)
  `(list ,@(mapcar #'mop-canonicalize-specializer specializers)))

(defun mop-canonicalize-specializer (specializer)
  `(find-class ',specializer))

(defun mop-parse-defmethod (args)
  (let ((fn-spec (car args))
        (qualifiers ())
        (specialized-lambda-list nil)
        (body ())
        (parse-state :qualifiers))
    (dolist (arg (cdr args))
      (ecase parse-state
        (:qualifiers (if (and (atom arg) (not (null arg)))
                         (mop-push-on-end arg qualifiers)
                         (progn (setq specialized-lambda-list arg)
                                (setq parse-state :body))))
        (:body (mop-push-on-end arg body))))
    (values fn-spec
            qualifiers
            (mop-extract-lambda-list specialized-lambda-list)
            (mop-extract-specializers specialized-lambda-list)
            (list* 'block
                   (if (consp fn-spec)
                       (cadr fn-spec)
                       fn-spec)
                   body))))

(defun mop-required-portion (gf args)
  (let ((number-required (length (mop-gf-required-arglist gf))))
    (when (< (length args) number-required)
      (error "Too few arguments to generic function ~S." gf))
    (subseq args 0 number-required)))

(defun mop-gf-required-arglist (gf)
  (let ((plist (mop-analyze-lambda-list (mop-generic-function-lambda-list gf))))
    (getf plist ':required-args)))

(defun mop-extract-lambda-list (specialized-lambda-list)
  (let* ((plist (mop-analyze-lambda-list specialized-lambda-list))
         (requireds (getf plist ':required-names))
         (rv (getf plist ':rest-var))
         (ks (getf plist ':key-args))
         (aok (getf plist ':allow-other-keys))
         (opts (getf plist ':optional-args))
         (auxs (getf plist ':auxiliary-args)))
    `(,@requireds
      ,@(if rv `(&rest ,rv) ())
      ,@(if (or ks aok) `(&key ,@ks) ())
      ,@(if aok '(&allow-other-keys) ())
      ,@(if opts `(&optional ,@opts) ())
      ,@(if auxs `(&aux ,@auxs) ()))))

(defun mop-extract-specializers  (specialized-lambda-list)
  (let ((plist (analyze-lambda-list specialized-lambda-list)))
    (getf plist ':specializers)))

(defun mop-analyze-lambda-list (lambda-list)
  (labels ((make-keyword (symbol)
             (intern (symbol-name symbol)
                     (find-package 'keyword)))
           (get-keyword-from-arg (arg)
             (if (listp arg)
                 (if (listp (car arg))
                     (caar arg)
                     (make-keyword (car arg)))
                 (make-keyword arg))))
    (let ((keys ())
          (key-args ())
          (required-names ())
          (required-args ())
          (specializers ())
          (rest-var nil)
          (optionals ())
          (auxs ())
          (allow-other-keys nil)
          (state :parsing-required))
      (dolist (arg lambda-list)
        (if (member arg lambda-list-keywords)
            (ecase arg
              (&optional (setq state :parsing-optional))
              (&rest (setq state :parsing-rest))
              (&key (setq state :parsing-key))
              (&allow-other-keys (setq allow-other-keys 't))
              (&aux (setq state :parsing-aux)))
            (case state
              (:parsing-required (mop-push-on-end arg required-args)
                                 (if (listp arg)
                                     (progn (mop-push-on-end (car arg) required-names)
                                            (mop-push-on-end (cadr arg) specializers))
                                     (progn (mop-push-on-end arg required-names)
                                            (mop-push-on-end 't specializers))))
              (:parsing-optional (mop-push-on-end arg optionals))
              (:parsing-rest (setq rest-var arg))
              (:parsing-key (mop-push-on-end (get-keyword-from-arg arg) keys)
                            (mop-push-on-end arg key-args))
              (:parsing-aux (mop-push-on-end arg auxs)))))
      (list :required-names required-names
            :required-args required-args
            :specializers specializers
            :rest-var rest-var
            :keywords keys
            :key-args key-args
            :auxiliary-args auxs
            :optional-args optionals
            :allow-other-keys allow-other-keys))))

(defun mop-ensure-method (gf &rest all-keys)
  (let ((new-method (apply (if (eq (mop-generic-function-method-class gf) mop-the-class-standard-method)
                               #'mop-make-instance-standard-method
                               #'mop-instance)
                           (mop-generic-function-method-class gf)
                           all-keys)))
    (mop-add-method gf new-method)
    new-method))

(defun mop-make-instance-standard-method (method-class &key lambda-list qualifiers specializers body environment)
  (declare (ignore method-class))
  (let ((method (mop-std-allocate-instance mop-the-class-standard-method)))
    (setf (mop-method-lambda-list method) lambda-list)
    (setf (mop-method-qualifiers method) qualifiers)
    (setf (mop-method-specializers method) specializers)
    (setf (mop-method-body method) body)
    (setf (mop-method-environment method) environment)
    (setf (mop-method-generic-function method) nil)
    (setf (mop-method-function method) (mop-std-compute-method-function method))
    method))

(defun mop-add-method (gf method)
  (let ((old-method (mop-find-method gf (mop-method-qualifiers method)
                                        (mop-method-specializers method) nil)))
    (when old-method (mop-remove-method gf old-method)))
  (setf (mop-method-generic-function method) gf)
  (push method (mop-generic-function-methods gf))
  (dolist (specializer (mop-method-specializers method))
    (pushnew method (mop-class-direct-methods specializer)))
  (mop-finalize-generic-function gf)
  method)

(defun mop-remove-method (gf method)
  (setf (mop-generic-function-methods gf) (remove method (mop-generic-function-methods gf)))
  (setf (mop-method-generic-function method) nil)
  (dolist (class (mop-method-specializers method))
    (setf (mop-class-direct-methods class) (remove method (mop-class-direct-methods class))))
  (mop-finalize-generic-function gf)
  method)

(defun mop-find-method (gf qualifiers specializers &optional (errorp t))
  (let ((method (find-if #'(lambda (method)
                             (and (equal qualifiers (mop-method-qualifiers method))
                                  (equal specializers (mop-method-specializers method))))
                         (mop-generic-function-methods gf))))
    (if (and (null method) errorp)
        (error "No such method for ~S." (mop-generic-function-name gf))
        method)))

(defun mop-add-reader-method (class fn-name slot-name)
  (mop-ensure-method
   (mop-ensure-generic-function fn-name :lambda-list '(object))
   :lambda-list '(object)
   :qualifiers ()
   :specializers (list class)
   :body `(mop-slot-value object ',slot-name)
   :environment (mop-top-level-environment))
  (values))

(defun mop-add-writer-method (class fn-name slot-name)
  (mop-ensure-method
   (mop-ensure-generic-function fn-name :lambda-list '(new-value object))
   :lambda-list '(new-value object)
   :qualifiers ()
   :specializers (list (mop-find-class 't) class)
   :body `(setf (mop-slot-value object ',slot-name) new-value)
   :environment (mop-top-level-environment))
  (values))

(defun mop-apply-generic-function (gf args)
  (apply (mop-generic-function-discriminating-function gf) args))

(defun mop-std-compute-discriminating-function (gf)
  #'(lambda (&rest args)
      (let* ((classes (mapcar #'class-of (mop-required-portion gf args)))
             (emfun (gethash (mop-classes-to-emf-table gf) nil)))
        (if emfun
            (funcall emfun args)
            (mop-slow-method-lookup gf args classes)))))

(defun mop-slow-method-lookup (gf args classes)
  (let* ((applicable-methods (mop-compute-applicable-methods-using-classes gf classes))
         (emfun (funcall (if (eq (mop-class-of gf) mop-the-class-standard-gf)
                             #'mop-std-compute-effective-method-function
                             #'mop-compute-effective-method-function)
                         gf applicable-methods)))
    (setf (gethash classes (mop-classes-to-emf-table gf)) emfun)
    (funcall emfun args)))

(defun mop-compute-applicable-methods-using-classes (gf required-classes)
  (sort (copy-list (remove-if-not #'(lambda (method)
                                      (every #'subclassp required-classes (mop-method-specializers method)))
                                  (mop-generic-function-methods gf)))
        #'(lambda (m1 m2)
            (funcall (if (eq (mop-class-of gf) mop-the-class-standard-gf)
                         #'mop-std-method-more-specific-p
                         #'mop-method-more-specific-p)
                     gf m1 m2 required-classes))))

(defun mop-std-method-more-specific-p (gf method1 method2 required-classes)
  (declare (ignore gf))
  (mapc #'(lambda (spec1 spec2 arg-class)
            (unless (eq spec1 spec2)
              (return-from mop-std-method-more-specific-p (sub-specializer-p spec1 spec2 arg-class))))
        (mop-method-specializers method1)
        (mop-method-specializers method2)
        required-classes)
  nil)

(defun mop-apply-methods (gf args methods)
  (funcall (mop-compute-effective-method-function gf methods) args))

(defun mop-primary-method-p (method)
  (null (mop-method-qualifiers method)))

(defun mop-before-method-p (method)
  (equal '(:before) (mop-method-qualifiers method)))

(defun mop-after-method-p (method)
  (equal '(:after) (mop-method-qualifiers method)))

(defun mop-around-method-p (method)
  (equal '(:around) (mop-method-qualifiers method)))

(defun mop-std-compute-effective-method-function (gf methods)
  (let ((primaries (remove-if-not #'mop-primary-method-p methods))
        (around (find-if #'mop-around-method-p methods)))
    (when (null primaries)
      (error "No primary methods for the generic function ~S." gf))
    (if around
        (let ((next-emfun (funcall (if (eq (mop-class-of gf) mop-the-class-standard-gf)
                                       #'mop-std-compute-effective-method-function
                                       #'mop-compute-effective-method-function)
                                   gf (remove around methods))))
          #'(lambda (args)
              (funcall (mop-method-function around) args next-emfun)))
        (let ((next-emfun (mop-compute-primary-emfun (cdr primaries)))
              (befores (remove-if-not #'mop-before-method-p methods))
              (reverse-afters (reverse (remove-if-not #'mop-after-method-p methods))))
          #'(lambda (args)
              (dolist (before befores)
                (funcall (mop-method-function before) args nil))
              (multiple-value-prog1
                  (funcall (mop-method-function (car primaries)) args next-emfun)
                (dolist (after reverse-afters)
                  (funcall (mop-method-function after) args nil))))))))

(defun mop-compute-primary-emfun (methods)
  (if (null methods)
      nil
      (let ((next-emfun (mop-compute-primary-emfun (cdr methods))))
        #'(lambda (args)
            (funcall (mop-method-function (car methods)) args next-emfun)))))

(defun mop-apply-method (method args next-methods)
  (funcall (mop-method-function method)
           args
           (if (null next-methods)
               nil
               (mop-compute-effective-method-function
                (mop-method-generic-function method) next-methods))))

(defun mop-std-compute-method-function (method)
  (let ((form (mop-methody-body method))
        (lambda-list (mop-method-lambda-list method)))
    (mop-compile-in-lexical-environment (method-environment method)
      `(lambda (args next-emfun)
         (flet ((call-next-method (&rest cnm-args)
                  (if (null next-emfun)
                      (error "No next method for the generic function ~S." (mop-method-generic-function ',method))
                      (funcall next-emfun (or cnm-args args))))
                (mop-next-method-p () (not (null next-emfun))))
           (apply #'(lambda ,(mop-kludge-arglist lambda-list) ,form) args))))))

(defun mop-kludge-arglist (lambda-list)
  (if (and (member '#key lambda-list)
           (not (member '&allow-other-keys lambda-list)))
      (append lambda-list '(&allow-other-keys))
      (if (and (not (member '&rest lambda-list))
               (not (member '&key lambda-list)))
          (append lambda-list '(&key &allow-other-keys))
          lambda-list)))

(defun mop-top-level-environment () nil)

(defun mop-compile-in-lexical-environment (env lambda-expr)
  (declare (ignore env))
  (compile nil lambda-expr))

;-------------------------------------------------------------------------------
; Bootstrap
;-------------------------------------------------------------------------------



;-------------------------------------------------------------------------------
; Definitions
;-------------------------------------------------------------------------------

(mop-defclass mop-standard-class ()
  ((name :initarg :name
         :accessor class-name)
   (direct-superclasses :initarg :direct-superclasses
                        :accessor class-direct-superclasses)
   (direct-slots :accessor class-direct-slots)
   (class-precedence-list :accessor class-precedence-list)
   (effective-slots :accessor class-slots)
   (direct-subclasses :initform ()
                      :accessor class-direct-subclasses)
   (direct-methods :initform ()
                   :accessor class-direct-methods)))

;-------------------------------------------------------------------------------
; Scratch...
;-------------------------------------------------------------------------------

(mop-find-class 'mop-standard-class)