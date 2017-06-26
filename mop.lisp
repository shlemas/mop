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

; Stub
(defun mop-make-instance (class-name &rest all-keys)
  (list class-name all-keys))

;-------------------------------------------------------------------------------
; defclass
;-------------------------------------------------------------------------------

(let ((mop-class-table (make-hash-table :test #'eq)))

  (defun mop-find-class (symbol &optional (errorp t))
    (let ((class (gethash symbol mop-class-table nil)))
      (if (and (null class) errorp)
          (error "No class named ~S." symbol)
          class)))

  (defun (setf mop-find-class) (new-value symbol)
    (setf (gethash symbol mop-class-table) new-value))

)

(defun mop-canonicalize-direct-superclass (class-name)
  `(find-class ',class-name))

(defun mop-canonicalize-direct-superclasses (direct-superclasses)
  `(list ,@(mapcar #'mop-canonicalize-direct-superclass direct-superclasses)))

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

(defun mop-canonicalize-direct-slots (direct-slots)
  `(list ,@(mapcar #'mop-canonicalize-direct-slot direct-slots)))

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

(defun mop-ensure-class (name &rest all-keys)
  (if (mop-find-class name nil)
      (error "Can't redefine the class named ~S." name)
      (let ((class (apply #'mop-make-instance 'mop-standard-class :name name all-keys)))
        (setf (mop-find-class name) class)
        class)))

(defmacro mop-defclass (name direct-superclasses direct-slots &rest options)
  `(mop-ensure-class ',name
     :direct-superclasses ,(mop-canonicalize-direct-superclasses direct-superclasses)
     :direct-slots ,(mop-canonicalize-direct-slots direct-slots)
     ,@(mop-canonicalize-defclass-options options)))

;-------------------------------------------------------------------------------
; defgeneric
;-------------------------------------------------------------------------------

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

(defun mop-canonicalize-defgeneric-option (option)
  (case (car option)
    (:generic-function-class (list ':generic-function-class `(find-class ',(cadr option))))
    (:method-class (list ':method-class `(find-class ',(cadr option))))
    (otherwise (list `',(car option) `',(cadr option)))))

(defun mop-canonicalize-defgeneric-options (options)
  (mop-mapappend #'mop-canonicalize-defgeneric-option options))

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

(defun finalize-generic-function (gf)
  (setf (mop-generic-function-discriminating-function gf)
        (funcall (if (eq (mop-class-of gf) mop-the-class-standard-gf)
                     #'mop-std-compute-discriminating-function
                     #'mop-compute-discriminating-function)
                 gf))
  (setf (fdefinition (mop-generic-function-name gf))
        (mop-generic-function-discriminating-function gf))
  (clrhash (mop-classes-to-emf-table gf))
  (values))

(defmacro mop-defgeneric (function-name lambda-list &rest options)
  `(mop-ensure-generic-function ',function-name
                                :lambda-list ',lambda-list
                                ,@(mop-canonicalize-defgeneric-options options)))

;-------------------------------------------------------------------------------
; Definitions.
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