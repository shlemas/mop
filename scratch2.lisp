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
  (let ((slots (class-slots instance))
        (direct-slots (sixth initargs)))
    (do ((remaining-slots slots (cdr remaining-slots)))
        ((null remaining-slots))
      (let* ((slot (car remaining-slots))
             (slot-initargs (find-if (lambda (args)
                                       (equal (slot-definition-name slot) (getf args ':name)))
                                     direct-slots)))
        (setf (slot-definition-publish slot) (getf slot-initargs ':publish nil))
        (setf (slot-definition-subscribe slot) (getf slot-initargs ':subscribe nil))
        (when (slot-definition-subscribe slot)
          (let ((subscribers (gethash (slot-definition-name slot) pub/sub-table (list))))
            (unless (assoc instance subscribers)
              (setf (gethash (slot-definition-name slot) pub/sub-table)
                    (cons (cons instance (slot-definition-subscribe slot)) subscribers)))))))))

(defmethod (setf slot-value-using-class) :after (new-value (class pub/sub-class) instance slot-name)
  (let ((slots (class-slots class)))
    (when (slot-definition-publish (find-if (lambda (slot)
                                              (equal (slot-definition-name slot) slot-name))
                                            slots))
      (print slot-name)
      (print "SHOULD BE PUBLISHED")))) ; TODO

; ------------------------------------------------------------------------------
; Example...
; ------------------------------------------------------------------------------

(defclass ps1 ()
  ((@foo :reader @foo :initform 100 :subscribe 'queue))
  (:metaclass pub/sub-class))

(defclass ps2 ()
  ((@bar :reader @bar :initform 200 :subscribe 'sample))
  (:metaclass pub/sub-class))

(defclass ps3 ()
  ((@baz :reader @baz :initform 300 :publish t))
  (:metaclass pub/sub-class))

(defclass ps4 ()
  ((@zip :reader @zip :initform 400))
  (:metaclass pub/sub-class))

(defvar i1 (make-instance 'ps1))
(defvar i2 (make-instance 'ps2))
(defvar i3 (make-instance 'ps3))
(defvar i4 (make-instance 'ps4))

; Print the pub/sub-table.
(maphash #'(lambda (key value)
             (print key)
             (print value))
         pub/sub-table)