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

; TODO: These are stubs...
(define canonicalize-direct-superclasses (make-keyword-procedure (lambda (kws kw-args . rest) (list #f))))
(define canonicalize-direct-slots (make-keyword-procedure (lambda (kws kw-args . rest) (list #f))))

; TODO: Handle #:metaclass and #:default-initargs options.
(define (canonicalize-defclass-option option value)
  (list option value))

(define canonicalize-defclass-options
  (make-keyword-procedure
   (lambda (kws kw-args)
     (flatten (map canonicalize-defclass-option kws kw-args)))))

(define class-table (make-hasheq))

(define (find-class name #:error (error? #t))
  (let ((class (hash-ref class-table name #f)))
    (if (and (not class) error?)
        (raise-user-error 'find-class "class does not exist: ~a" name)
        class)))

(define (add-class name class)
  (hash-set! class-table name class))

; This is a generic function in the book because it implements CLOS in CLOS.
; For us, it will be a regular function.
; TODO: make-instance will need to call the canonicalize-* methods:
;         * canonicalize-direct-superclasses
;         * canonicalize-direct-slots
;         * canonicalize-defclass-options
(define make-instance
  (make-keyword-procedure
   (lambda (kws kw-args class . rest)
     `(make-instance ,class ,@rest ,@(map cons kws kw-args)))))

; TODO: How will canonicalize-defgeneric-options be handled?
(define-syntax-rule (defgeneric function-name lambda-list option ...)
  (begin
    (define function-name (make-instance 'standard-generic-function #:lambda-list lambda-list option ...))
    function-name))

(define-syntax-rule (defclass name direct-superclasses direct-slots option ...)
  (if (find-class 'name #:error #f)
      (raise-user-error 'defclass "cannot redefine class: ~a" 'name)
      (let ((class (make-instance 'standard-class
                                  #:name 'name
                                  #:direct-superclasses direct-superclasses
                                  #:direct-slots direct-slots
                                  option ...)))
        (add-class 'name class)
        class)))

; Scratch...
(defclass hey (list 'foo) (list 'bar) #:beep 100 #:bop 200)
(defgeneric baz (list 1 2 3))
baz