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