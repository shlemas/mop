# Metaobject Protocol

This repo holds my code for implementing a metaobject protocol and object system similar to the [Common Lisp Object System (CLOS)](https://en.wikipedia.org/wiki/Common_Lisp_Object_System) in [Racket](https://racket-lang.org) as I read through [The Art of the Metaobject Protocol](https://mitpress.mit.edu/books/art-metaobject-protocol).

The metaobject protocol defines the interfaces for defining the behaviors of:

* Classes
* Class instantiations (objects)
* Generic functions
* Methods

At the end, we should have a complete object system supporting:

* defclass
* defgeneric
* defmethod
