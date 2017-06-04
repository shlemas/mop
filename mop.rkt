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
(define make-instance (make-keyword-procedure (lambda (kws kw-args . rest) #f)))
(define canonicalize-direct-superclasses (make-keyword-procedure (lambda (kws kw-args . rest) (list #f))))
(define canonicalize-direct-slots (make-keyword-procedure (lambda (kws kw-args . rest) (list #f))))
(define canonicalize-defclass-options (make-keyword-procedure (lambda (kws kw-args . rest) (list #f))))

(define class-table (make-hasheq))

(define (find-class name #:error (error? #t))
  (let ((class (hash-ref class-table name #f)))
    (if (and (not class) error?)
        (raise-user-error 'find-class "class does not exist: ~a" name)
        class)))

(define (add-class name class)
  (hash-set! class-table name class))

; TODO: Merge options with kws and kw-args?
(define ensure-class
  (make-keyword-procedure
   (lambda (kws kw-args name options)
     (if (find-class name #:error #f)
      (raise-user-error 'ensure-class "cannot redefine class: ~a" name)
      (let ((class (keyword-apply make-instance kws kw-args 'standard-class options #:name name)))
        (add-class name class)
        class)))))

(define-syntax-rule (defclass name direct-superclasses direct-slots option ...)
  (ensure-class name
                #:direct-superclasses (canonicalize-direct-superclasses direct-superclasses)
                #:direct-slots (canonicalize-direct-slots direct-slots)
                (canonicalize-defclass-options option ...)))