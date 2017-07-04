; ------------------------------------------------------------------------------
; Load the Closette package.
; ------------------------------------------------------------------------------

(require 'asdf)
(ccl::cd (make-pathname :directory (pathname-directory (pathname *load-pathname*))))
(load "closette.asd")
(asdf:operate 'asdf:load-op 'closette)
(in-package #:closette)

; ------------------------------------------------------------------------------
; Scratch...
; ------------------------------------------------------------------------------

