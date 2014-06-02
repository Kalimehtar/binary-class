#lang scribble/manual
@(require (for-label racket binary-class))

@title{Binary class for parsing and saving binary data}
@author{@(author+email "Roman Klochkov" "kalimehtar@mail.ru")}

@(defmodule binary-class)

@section{Main}

This package is based upon idea in 
@(hyperlink "http://www.gigamonkeys.com/book/practical-parsing-binary-files.html" 
            "Practical Common Lisp").

Binary formats usually are represented as a sequence of fields. So the module allows to define 
classes with their binary types.

For example, ID3 tag is specified like this:
@verbatim|{
  ID3/file identifier      "ID3"
  ID3 version              $02 00
  ID3 flags                %xx000000
  ID3 size             4 * %0xxxxxxx}|

It may be represented as
@racketblock[
(define-binary-class id3-tag
  ((file-identifier (iso-8859-1-bytes 3))
   (major-version   u1)
   (revision        u1)
   (flags           u1)
   (size            id3-tag-size)
   (frames          (id3-frames size))))
]

Here @racket[iso-8859-1-bytes] should be a function of one argument, that returns structure 
@racket[binary]. @racket[u1], @racket[id3-tag-size] are simply such structures.

@defstruct[binary ([read (input-port? . -> . any)] 
                   [write (output-port? any/c . -> . void?)])]{
  A strucure type for binary values. Read is a function, that reads from input port and
returns the data. Write -- takes output-port and data to write the data in port.
}

Note, that you may use values of previous fields to calculate the type of the next. 
In the example the value of field @racket[size] is used to set the type of field @racket[frames].

Another common case is "tagged structures". In that case there is a tag in the beginning of data.
The structure of the rest of the data depends upon the value of the tag.

To accomplish the goal one may use @racket[#:dispatch] option.
@racketblock[
(define-binary-class id3-frame
  ((id     (iso-8859-1-bytes 3))
   (size   u3))
  #:dispatch (find-frame-class id))
]

Function @racket[find-frame-class] should return the binary class for given @racket[id].

You may even insert any expression after @racket[#:dispatch]
@racketblock[
(define-binary-class id3-tag
  ((identifier     (iso-8859-1-bytes 3))
   (major-version  u1)
   (revision       u1)
   (flags          u1)
   (size           id3-tag-size))
  #:dispatch 
   (case major-version
     ((2) id3v2.2-tag)
     ((3) id3v2.3-tag)))
]


Besides, you may use inheritance: simply add super class after the class name.
@racketblock[
(define-binary-class id3v2.2-tag id3-tag
  ((frames (id3-frames :tag-size size :frame-type 'id3v2.2-frame))))
]

If you use @racket[#:dispatch], result class should be either inherited from current class, or 
at least to have all fields that the current class has. Super class of a binary class may 
be also not binary class, but then it should have no methods @racket[read] and @racket[write].

@defform[(define-binary-class id [superclass-expr]
           ((field-id field-expr) ...) 
           [#:dispatch dispatch-expr])
          #:contracts ([superclass-expr class?] 
                       [field-expr binary?]
                       [dispatch-expr (implementation?/c binary<%>)])]{
  Defines new binary class and binds it to @racket[_id].}
                                                                     
Binary class implements interface @racket[binary<%>]:

@definterface[binary<%> (read write)]{
  @defmethod[(read [in input-port?]) (instanceof/c (implementation?/c binary<%>))]{
     Reads the object from @racket[in] and returns it.}
  @defmethod[(write [out output-port?]) void?]{
     Writes the object to @racket[out]}}

@section{Utilities}

To make the usage of the module easier there are some shortcuts 
for reading and writing binary values.

@defproc[(read-value [type binary?] [in input-port?]) any/c]{
Reads binary value from input port and returns it.}

@defproc[(write-value [type binary?] [out output-port?]) void?]{
Writes binary value to output port.}

@defproc[(read-object [binary-class (implementation?/c binary<%>)] 
                      [in input-port?]
                      [init-v any/c] ...)
         (instanceof/c (implementation?/c binary<%>))]{
Creates binary object, fills it from the input port and returns it. The @racket[init-v]s 
are passed as initialization arguments to @racket[make-object].}
