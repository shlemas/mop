; ------------------------------------------------------------------------------
; Load the Closette package.
; ------------------------------------------------------------------------------

(require 'asdf)
(ccl::cd (make-pathname :directory (pathname-directory (pathname *load-pathname*))))
(load "closette.asd")
(asdf:operate 'asdf:load-op 'closette)
(in-package :closette)

; ------------------------------------------------------------------------------
; Scratch...
; ------------------------------------------------------------------------------

(defclass pub/sub-class (standard-class)
  ((pub/sub-table :initform (make-hash-table :test #'eq))))

(defun slot-definition-attributes (slot)
  (getf slot ':attributes ()))
(defun (setf slot-definition-attributes) (new-value slot)
  (setf (getf* slot ':attributes) new-value))

(defun slot-attribute-bucket (instance slot-name attribute)
  (let* ((all-buckets (slot-value instance 'all-attributes))
         (slot-bucket (assoc slot-name all-buckets)))
    (unless slot-bucket
      (error "The slot named ~A of ~S has no attributes."
             slot-name instance))
    (let ((attr-bucket (assoc attribute (cdr slot-bucket))))
      (unless attr-bucket
        (error "The slot named ~A of ~S has no attribute~@
                named ~A." slot-name instance attribute))
      attr-bucket)))

(defun slot-attribute (instance slot-name attribute)
  (cdr (slot-attribute-bucket instance slot-name attribute)))

(defun (setf slot-attribute) (new-value instance slot-name attribute)
  (setf (cdr (slot-attribute-bucket
               instance slot-name attribute))
        new-value))

(defmethod compute-slots ((class pub/sub-class))
  (let* ((normal-slots (call-next-method))
         (alist (mapcar #'(lambda (slot)
                            (cons (slot-definition-name slot)
                                  (mapcar #'(lambda (attr) (cons attr nil))
                                          (slot-definition-attributes slot))))
                        normal-slots)))
    (cons (make-effective-slot-definition
           :name 'all-attributes
           :initform `',alist
           :initfunction #'(lambda () alist))
          normal-slots)))

(defmethod compute-effective-slot-definition ((class pub/sub-class)
                                              name
                                              direct-slots)
  (let ((normal-slot (call-next-method)))
    (setf (slot-definition-attributes normal-slot)
          (remove-duplicates
           (mapappend #'slot-definition-attributes direct-slots)))
    normal-slot))

(defclass zany ()
  ((bar :attributes (subscribe)))
  (:metaclass pub/sub-class))

(defclass credit-rating2 ()
  ((level :attributes (date-set time-set)))
  (:metaclass pub/sub-class))