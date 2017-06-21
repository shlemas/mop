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
; Behavior/support/utility layer.
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
    (mop-push-on-end new-value plit)
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
            (mapplist fun (cddr x)))))

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
                                                  (mapplist #'(lambda (key value)
                                                                `(',key ,value))
                                                            (cdr option))))))
    (otherwise (list `',(car option) `',(cadr option)))))

(defun mop-canonicalize-defclass-options (options)
  (mop-mapappend #'mop-canonicalize-defclass-option options))

; Stub
(defun mop-make-instance (class-name &rest all-keys)
  (list class-name all-keys))

;-------------------------------------------------------------------------------
; Glue layer.
;-------------------------------------------------------------------------------

(defun mop-ensure-class (name &rest all-keys)
  (if (mop-find-class name nil)
      (error "Can't redefine the class named ~S." name)
      (let ((class (apply #'mop-make-instance 'mop-standard-class :name name all-keys)))
        (setf (mop-find-class name) class)
        class)))

;-------------------------------------------------------------------------------
; Macro-expansion layer.
;-------------------------------------------------------------------------------

(defmacro mop-defclass (name direct-superclasses direct-slots &rest options)
  `(mop-ensure-class ',name
     :direct-superclasses ,(mop-canonicalize-direct-superclasses direct-superclasses)
     :direct-slots ,(mop-canonicalize-direct-slots direct-slots)
     ,@(mop-canonicalize-defclass-options options)))

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