; ------------------------------------------------------------------------------
; Load the Closette package.
; ------------------------------------------------------------------------------

(require 'asdf)
(ccl::cd (make-pathname :directory (pathname-directory (pathname *load-pathname*))))
(load "closette.asd")
(asdf:operate 'asdf:load-op 'closette)
(in-package :closette)

; ------------------------------------------------------------------------------
; Expand standard-class with publish/subscribe mechanism.
; ------------------------------------------------------------------------------

(defvar pub/sub-table (make-hash-table :test #'eq))
(defvar class-object-table (make-hash-table :test #'eq))
(defvar should-publish? t)

(defclass pub/sub-class (standard-class) ())

(defun slot-definition-publish (slot)
  (getf slot ':publish nil))
(defun (setf slot-definition-publish) (new-value slot)
  (setf (getf* slot ':publish) new-value))

(defun slot-definition-subscribe (slot)
  (getf slot ':subscribe nil))
(defun (setf slot-definition-subscribe) (new-value slot)
  (setf (getf* slot ':subscribe) new-value))

(defmethod compute-effective-slot-definition ((class pub/sub-class)
                                              direct-slots)
  (let ((normal-slot (call-next-method)))
    (setf (slot-definition-publish normal-slot) nil)
    (setf (slot-definition-subscribe normal-slot) nil)
    normal-slot))

(defmethod initialize-instance :after ((instance pub/sub-class) &rest initargs)
  (print "initialize-instance :after")
  (let* ((slots (class-slots instance))
         (direct-slots (sixth initargs)))
    (do ((remaining-slots slots (cdr remaining-slots)))
        ((null remaining-slots))
      (let* ((slot (car remaining-slots))
             (slot-name (slot-definition-name slot))
             (slot-initargs (find-if (lambda (args)
                                       (equal slot-name (getf args ':name)))
                                     direct-slots)))
        (setf (slot-definition-publish slot) (getf slot-initargs ':publish nil))
        (setf (slot-definition-subscribe slot) (getf slot-initargs ':subscribe nil))
        (when (slot-definition-subscribe slot)
          (let ((subscribers (gethash slot-name pub/sub-table (list))))
            (unless (assoc instance subscribers)
              (setf (gethash slot-name pub/sub-table)
                    (cons (list instance (slot-definition-subscribe slot)) subscribers))))))))
  instance)

(defmethod make-instance ((class pub/sub-class) &rest initargs)
  (print "make-instance")
  (setf should-publish? nil)
  (let ((instance (call-next-method))
        (objects (gethash class class-object-table (list))))
    (setf (gethash class class-object-table) (cons instance objects))
    (setf should-publish? t)
    instance))

(defmethod (setf slot-value-using-class) :after (new-value (class pub/sub-class) instance slot-name)
  (print "setf :after")
  (when should-publish?
    (setf should-publish? nil)
    (let ((class-subscribers (gethash slot-name pub/sub-table (list))))
      (do ((cs class-subscribers (cdr cs)))
          ((null cs))
        (let ((subscribers (gethash (caar cs) class-object-table (list)))
              (subscription-type (cadar cs)))
          (do ((s subscribers (cdr s)))
              ((null s))
            (let ((current-slot-value (slot-value (car s) slot-name)))
              (if (eq subscription-type 'queue)
                  (push-on-end new-value current-slot-value)
                  (setf (slot-value (car s) slot-name) new-value)))))))
    (setf should-publish? t)))

; ------------------------------------------------------------------------------
; Example...
; ------------------------------------------------------------------------------

(defclass ps1 ()
  ((@foo :reader @foo :initform (list 100) :subscribe queue))
  (:metaclass pub/sub-class))

(defclass ps2 ()
  ((@foo :reader @foo :initform 200 :subscribe sample))
  (:metaclass pub/sub-class))

(defclass ps3 ()
  ((@foo :reader @foo :initform 300 :publish t))
  (:metaclass pub/sub-class))

(defclass ps4 ()
  ((@zip :reader @zip :initform 400))
  (:metaclass pub/sub-class))

(defvar i1 (make-instance 'ps1))
(defvar i2 (make-instance 'ps2))
(defvar i3 (make-instance 'ps3))
(defvar i4 (make-instance 'ps4))

; Print the pub/sub-table.
(print "pub/sub-table")
(maphash #'(lambda (key value)
             (print key)
             (print value))
         pub/sub-table)

(print "class-object-table")
(maphash #'(lambda (key value)
             (print key)
             (print value))
         class-object-table)

(setf (slot-value i3 '@foo) 111)
(setf (slot-value i3 '@foo) 222)
(setf (slot-value i3 '@foo) 333)

(print "slot values")
(print (slot-value i1 '@foo))
(print (slot-value i2 '@foo))
(print (slot-value i3 '@foo))