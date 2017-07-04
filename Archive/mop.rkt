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

#lang racket

(require syntax/parse/define)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Behavior/support layer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: These are stubs...
(define (canonicalize-direct-superclass superclass) superclass)
(define (canonicalize-direct-slot slot) slot)

; TODO: Handle #:metaclass and #:default-initargs options.
(define (canonicalize-defclass-option option value) value)

(define (allocate-std-instance class slots)
  (list (list '#:class class)
        (list '#:slots slots)))

(define (std-instance-class instance)
  (cdr (assoc instance '#:class)))

(define (std-instance-slots instance)
  (cdr (assoc instance '#:slots)))

(define (std-instance? object)
  (if (std-instance-class object) #t #f))

(define (allocate-slot-storage size initial-value)
  (build-list size (lambda (ignored) initial-value)))

(define (slot-contents slots location)
  (list-ref slots location))

(define secret-unbound-value (list "slot unbound"))

;(define (class-slots class)
;  (slot-value class '#:effective-slots))

;(define (std-allocate-instance class)
;  (allocate-std-instance
;   class
;   (allocate-slot-storage (class-slots class) secret-unbound-value)))

;(define (allocate-instance class)
;  (std-allocate-instance class))

; This is a generic function in the book because it implements CLOS in CLOS.
; For us, it will be a regular function.
(define (make-instance class . options)
  `(make-instance ,class ,options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Glue layer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; UNUSED
(define class-table (make-hasheq))

; UNUSED
(define (find-class name #:error (error? #t))
  (let ((class (hash-ref class-table name #f)))
    (if (and (not class) error?)
        (raise-user-error 'find-class "class does not exist: ~a" name)
        class)))

; UNUSED
(define (add-class name class)
  (hash-set! class-table name class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Macro-expansion layer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (defgeneric function-name lambda-list (~seq option-name option-value) ...)
  (define function-name (make-instance 'standard-generic-function
                                       (cons '#:lambda-list 'lambda-list)
                                       (cons 'option-name option-value)
                                       ...)))

(define-simple-macro (defclass name (direct-superclass ...) (direct-slot ...) (~seq option-name option-value) ...)
  (define name (make-instance 'name
                              (cons '#:name 'name)
                              (cons '#:direct-superclasses (list (canonicalize-direct-superclass 'direct-superclass) ...))
                              (cons '#:direct-slots (list (canonicalize-direct-slot 'direct-slot) ...))
                              (cons 'option-name (canonicalize-defclass-option 'option-name option-value))
                              ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass standard-class ()
  ((name #:initarg #:name
         #:accessor class-name)
   (direct-superclasses #:initarg #:direct-superclasses
                        #:accessor class-direct-superclasses)
   (direct-slots #:accessor class-direct-slots)
   (class-precedence-list #:accessor class-precedence-list)
   (effective-slots #:accessor class-slots)
   (direct-subclasses #:initform ()
                      #:accessor class-direct-subclasses)
   (direct-methods #:initform ()
                   #:accessor class-direct-methods)))
; TODO
;(defmethod initialize-instance #:after ((class standard-class) &key direct-superclasses direct-slots)
;  (let ((supers (or (get '#:direct-superclasses options) standard-object)))
;    (setf (class-direct-superclasses class) supers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scratch...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

standard-class