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

; TODO: These are stubs...
(define (canonicalize-direct-superclass superclass) superclass)
(define (canonicalize-direct-slot slot) slot)

; TODO: Handle #:metaclass and #:default-initargs options.
(define (canonicalize-defclass-option option value) value)

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

; This is a generic function in the book because it implements CLOS in CLOS.
; For us, it will be a regular function.
(define (make-instance class . options)
  `(make-instance ,class ,options))

(define-simple-macro (defgeneric function-name lambda-list (~seq option-name option-value) ...)
  (define function-name (make-instance 'standard-generic-function
                                       (list '#:lambda-list 'lambda-list)
                                       (list 'option-name option-value)
                                       ...)))

(define-simple-macro (defclass name (direct-superclass ...) (direct-slot ...) (~seq option-name option-value) ...)
  (define name (make-instance 'standard-class
                              (list '#:name 'name)
                              (list '#:direct-superclasses (list (canonicalize-direct-superclass 'direct-superclass) ...))
                              (list '#:direct-slots (list (canonicalize-direct-slot 'direct-slot) ...))
                              (list 'option-name (canonicalize-defclass-option 'option-name option-value))
                              ...)))

; Scratch...
(defclass hey (heyparent1 heyparent2)
  (#:bar #:zing)
  #:beep 100
  #:bop 200)

(defgeneric baz (arg1 arg2 arg3) #:initarg1 "blah")

hey
baz