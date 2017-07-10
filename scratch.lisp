(defclass pub/sub-class (standard-class)
  ((pub/sub-table :initform (make-hash-table :test #'eq) :allocation :class)))

(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

(defun (setf getf*) (new-value plist key)
  (block body
    (do ((x plist (cddr x)))
        ((null x))
      (when (eq (car x) key)
        (setf (car (cdr x)) new-value)
        (return-from body new-value)))
    (push-on-end key plist)
    (push-on-end new-value plist)
    new-value))

(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))

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

(defun slot-attribute (instance slot-name attribute)
  (cdr (slot-attribute-bucket instance slot-name attribute)))

(defun (setf slot-attribute) (new-value instance slot-name attribute)
  (setf (cdr (slot-attribute-bucket instance slot-name attribute)) new-value))

(defun slot-attribute-bucket (instance slot-name attribute)
  (let* ((all-buckets (slot-value instance 'all-attributes))
         (slot-bucket (assoc slot-name all-buckets)))
    (unless slot-bucket
      (error "The slot named ~S of ~S has no attributes." slot-name instance))
    (let ((attr-bucket (assoc attribute (cdr slot-bucket))))
      (unless attr-bucket
        (error "The slot named ~S of ~S has no attribute named ~S." slot-name instance attribute))
      attr-bucket)))

(defun slot-definition-attributes (slot)
  (getf slot ':attributes))

(defun (setf slot-definition-attributes) (new-value slot)
  (setf (getf* slot ':attributes) new-value))

;(defmethod initialize-instance :after ((instance pub/sub-class) &rest initargs)
;  (print "hey"))

;(defmethod initialize-instance :after ((instance pub/sub-class) &rest initargs)
;  (let ((slots (class-slots (class-of instance)))
;        (pub/sub-table (slot-value instance 'pub/sub-table)))
;    (print "hey")
;    (do ((s slots (cdr s)))
;        ((null s))
;      (when (eq ((slot-definition-name (car s)) 'pub/sub-table))
;        (print (slot-attribute instance (car s) 'subscribe))))))
;      ;(incf (gethash 'foo pub/sub-table :default 0))))

(defclass pub/sub-subclass1 (pub/sub-class)
  (@foo :attributes (subscribe)))
;(defclass pub/sub-subclass2 (pub/sub-class)
;  (@foo :publish))

;(defvar foo1 (make-instance 'pub/sub-subclass1))
;(defvar foo2 (make-instance 'pub/sub-subclass2))

;(print (slot-value foo1 'pub/sub-table))
;(print (slot-value foo2 'pub/sub-table))

;(setf (gethash 'foo (slot-value foo1 'pub/sub-table)) 1)
;(setf (slot-value (slot-value foo1 'pub/sub-table) 'subscribe) "okay")

;(print (slot-value foo1 'pub/sub-table))
;(print (slot-value foo2 'pub/sub-table))

;(defmethod compute-slots ((class pub/sub-class))
;  (let ((normal-slots (call-next-method)))
;    (cons (make-effective-slot-definition
;           :name 'publish
;           :initform nil
;           :initfunction #'(lambda () nil))
;          (cons (make-effective-slot-definition
;                 :name 'subscribe
;                 :initform nil
;                 :initfunction #'(lambda () nil))
;                normal-slots))))