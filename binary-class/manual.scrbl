#lang scribble/manual
@(require (for-label racket binary-class))

@title{Binary-class: parsing and saving binary data}
@author{@(author+email "Roman Klochkov" "kalimehtar@mail.ru")}

@(defmodule binary-class)

The @racket[binary-class] combines @racket[binary-class/base], @racket[binary-class/common] and
@racket[binary-class/string].

@section{Binary class. Base system.}

@(defmodule binary-class/base)

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
  ((frames (id3-frames size id3v2.2-frame))))
]

If you use @racket[#:dispatch], result class should be either inherited from current class, or 
at least to have all fields that the current class has. Super class of a binary class may 
be also not binary class, but then it should have no methods @racket[read] and @racket[write].

@defform/subs[
(define-binary-class id [superclass-expr]
  ((field-id field-expr) ...) 
  [#:dispatch dispatch-expr]
  class-body ...)
([field-id _ id])
#:contracts ([superclass-expr class?] 
             [field-expr binary?]
             [dispatch-expr (is-a?/c binary<%>)])]{
Defines new binary class and binds it to @racket[_id]. @racket[class-body] --- any definitions, 
allowed inside @racket[class]
                                         
@racket[field-id] may be @racket[__]. This means, that the field is omitted. 
In this case no field is created in class, but the data is read and is written 
from/to the binary port. Value for writing is @racket[#f].

@racket[_superclass-expr] may be either id of a binary class, or any expression, 
returning non-binary class. If you return binary class from expression, then it is not error,
but fields of given class will not be visible inside the current class @racket[field-expr]s.}

Binary class implements interface @racket[binary<%>]:

@definterface[binary<%> (read write)]{
  @defmethod[(read [in input-port?]) (is-a?/c binary<%>)]{
     Reads the object from @racket[in] and returns it.}
  @defmethod[(write [out output-port?]) void?]{
     Writes the object to @racket[out]}}

@subsection{Utilities}

To make the usage of the module easier there are some shortcuts 
for reading and writing binary values.

@defproc[(read-value [type binary?] [in input-port?]) any]{
Reads binary value from input port and returns it.}

@defproc[(write-value [type binary?] [out output-port?] [value any/c]) void?]{
Writes binary value to output port.}

@defproc[(read-object [binary-class (implementation?/c binary<%>)] 
                      [in input-port?]
                      [init-v any/c] ...)
         (is-a?/c binary<%>)]{
Creates binary object, fills it from the input port and returns it. The @racket[init-v]s 
are passed as initialization arguments to @racket[make-object].}

@section{Common datatypes}

@(defmodule binary-class/common)

Most common data in binary file is integer numbers in little-endian or big-endian order, 
or bytestrings. So you may use them from this module.

@defproc[(unsigned-integer [bytes exact-positive-integer?] 
                           [bits-per-byte exact-positive-integer? 8])
         binary?]{Returns binary datatype for unsigned integer with big-endian order}

@defproc[(unsigned-integer-le [bytes exact-positive-integer?] 
                              [bits-per-byte exact-positive-integer? 8])
         binary?]{Returns binary datatype for unsigned integer with little-endian order}

@deftogether[(@defthing[u1 binary?]
              @defthing[u2 binary?]
              @defthing[u3 binary?]
              @defthing[u4 binary?]
              @defthing[l1 binary?]
              @defthing[l2 binary?]
              @defthing[l3 binary?]
              @defthing[l4 binary?])]{
Binary types for big-endian @racket[u1] -- @racket[u4] and @racket[l1] -- @racket[l4] 
little-edian ones. Number 1--4 displays the length of the integer in bytes}

@defproc[(discard [bytes exact-positive-integer?]) binary?]{
Reads given number of bytes and return @racket[#f]. Writes given number of null bytes.
Recommended for use with field id @racket[_] when you see "Reserved" in the specification.}

@defproc[(bytestring [bytes exact-positive-integer?]) binary?]{
Reads and writes bytes to @racket[bytes?] from binary port and vice versa}.

@section{Strings}

@(defmodule binary-class/string)

In this module there are several binary types for reading and writing @racket[string?].

@defproc[(generic-string [length exact-positive-integer?] [character-type binary?]) binary?]{
Returns type, describing string with given fixed @racket[length] and @racket[character-type],
that describes how to read and write every @racket[char?].}

@defproc[(generic-terminated-string [terminator char?] [character-type binary?]) binary?]{
Returns type, describing string with given @racket[terminator] and @racket[character-type].
@racket[terminator] is present in file, but not in Racket string.}

@defproc[(iso-8859-1-string [length exact-positive-integer?]) binary?]{
String, represented in file as a ISO 8859-1 string with fixed @racket[length]. 
Only @racket[char?]'s with codes up to 255 allowed}

@defproc[(iso-8859-1-terminated-string [terminator char? #\nul]) binary?]{
String, represented in file as a ISO 8859-1 string with @racket[terminator].
Only @racket[char?]'s with codes up to 255 allowed}

@defproc[(ucs-2-string [length exact-positive-integer?]) binary?]{
String, represented in file as a UCS-2 string with fixed @racket[length]. 
Only @racket[char?]'s with codes up to 65535 allowed}

@defproc[(ucs-2-terminated-string [terminator char? #\nul]) binary?]{
String, represented in file as a UCS-2 string with @racket[terminator].
Only @racket[char?]'s with codes up to 65535 allowed}

@section{Performance and safety}

By default contracts in @racket[binary-class]/* check only function arguments. If you need more
security or more performance, you may use instead their submodules: Submodule @racket[safe]
gives maximum safety and contract checks, submodule @racket[unsafe] gives maximum performance,
but no check at all.

@(defmodule #:multi ((submod binary-class safe)
                     (submod binary-class unsafe)
                     (submod binary-class/base safe)
                     (submod binary-class/base unsafe)
                     (submod binary-class/common safe)
                     (submod binary-class/common unsafe)
                     (submod binary-class/string safe)
                     (submod binary-class/string unsafe)))

@section{Contracts}

@(defmodule binary-class/contract)

@defproc[(binary-integer/c [bytes exact-integer?] [bits-per-byte 8]) flat-contract?]{
Defines contract for @racket[unsigned-integer] with given @racket[_bytes] and @racket[_bits-per-byte]}

@defform/subs[
#:literals (field init init-field inherit inherit-field super inner override augment augride absent)

(class/c binary-class-id maybe-opaque member-spec ...)

([maybe-opaque
  (code:line)
  (code:line #:opaque)]

 [member-spec
  method-spec
  (field field-spec ...)
  (init field-spec ...)
  (init-field field-spec ...)
  (inherit method-spec ...)
  (inherit-field field-spec ...)
  (super method-spec ...)
  (inner method-spec ...)
  (override method-spec ...)
  (augment method-spec ...)
  (augride method-spec ...)
  (absent absent-spec ...)]

 [method-spec
  method-id
  (method-id method-contract-expr)]
 [field-spec
  field-id
  (field-id contract-expr)]
 [absent-spec
  method-id
  (field field-id ...)])]{

Defines contract for binary class. @racket[_binary-class-id] should be an id of existing binary class.
Rest arguments are the same as for @racket[class/c].}