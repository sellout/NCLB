;@
(in-package #:nclb)
;@

#|@
# Introduction

This is _NCLB_ (No Coder Left Behind), a literate programming system. It is
less like _CWEB_ than anything I've seen. The _CWEB_ manual says “[w]riting
CWEB programs is something like writing TEX documents, but with an additional
‘C mode’ that is added to TEX’s horizontal mode, vertical mode, and math mode.”
In contrast, _NCLB_ removes the idea of a “web” and all files are either valid documentation files or valid code files, with the operations `tangle` and `weave` simply converting one to the other. In a code file, the documentation is contained in specially-marked comments and in a documentation file, the code is in designated code sections. There are a few reasons for this:

1. with the documentation in comments, there is no need for the `tangle`
   operation, since the original file is executable (this is especially useful
   for interpreted and interactive languages, where `tangle` can not easily be
   hidden inside some existing build process);
2. code that is written in a literate style does not need to be handled
   differently from code written without literate sections; and
3. literate programming, in practice, is mostly used for things like tutorials,
   manuals, and books, with most other code having little or no use for it.

To dive more into that third point, it has become apparent that over-documenting
code (although a noble attempt to explain things) invariably results in
documentation that does not describe the code. There are of course still
comments, but they are never written in a literate style. Part of the goal with
this system is to encourage all developer documentation to be written in a
literate style by removing a few of the friction points that prevent developers
from using most literate systems:

1. all users are required to install your literate toolset in addition to the
   usual language toolset,
2. LaTeχ can be hard to read in source format – for those users who don’t or
   can’t weave the web into a document (especially when the LaTeχ sections are
   little more than short comments), and
3. managing sections requires a new set of syntaxes and ones that are more
   verbose than commenting usually is.

Point two is addressed by using Markdown _by default_ instead of LaTeχ. LaTeχ is
useful when writing a book, but less so for Web tutorials or comments. If you
are going to add the verbosity of LaTeχ, adding `;@latex` to your file should
hardly be an obstacle.

The one tradeoff that occurs to me with this approach is that macros can not
rearrange code, since there is no tangling process. The code needs to be written
in an order the compiler can process. However, macros can rearrange sections
during the weaving process, so it is hopefully not too big of a problem to
overcome.

;@@ merge this in somewhere earlier

Literate programming is useful to people who are reading the code – either developers working on it or users learning from it. In the former case, less is better, because those who change the code often ignore the docs while those who try to understand the system often ignore the code – if the two aren't in sync, trouble brews.

Documentation for users of a system belong in a combination of docstrings and literate tutorials & test cases. E.g., this literate document is part of a tutorial on _NCLB_, not part of the _NCLB_ source code. Tutorials and the like benefit from literate programming because the code sections can be compiled and tested and the text sections will be consumed as the primary content and potentially checked by editors.

Okay, so there _is_ a `tangle`, but it’s not used on the same original file. `tangle` converts valid documentation to valid code. There is still no “web” file, there are only code and documentation files. `weave` converts from code to documentation and `tangle` converts from documentation to code. At least in their current state, they are not isomorphic, but it does seem desirable that

> tangle(weave(x)) = x
@|#

(defun tangle (doc-stream code-stream)
  "Takes an input stream containing docs with code sections and an output
   stream to write code to."
  (let ((state 'doc))
    (loop for line = (read-line doc-stream nil)
       while line
       do (cond ((and (eq state 'doc) (string= line "```"))
                 (setf state 'code))
                ((and (eq state 'code) (string= line "```"))
                 (setf state 'doc))
                ((and (eq state 'doc) (string= line "<!--"))
                 (write-line ";@" code-stream)
                 (setf state 'skip))
                ((and (eq state 'skip) (string= line "-->"))
                 (write-line ";@" code-stream)
                 (setf state 'doc))
                ((eq 0 (search "<!--" line))
                 (write-line (concatenate 'string
                                          ";@@"
                                          (subseq line 4 (- (length line) 4)))
                             code-stream))
                ;; ((eq 0 (search "> " line))
                ;;  (write-line (subseq line 3) code-stream))
                ((eq state 'code)
                 (write-line line code-stream))
                ((eq state 'doc)
                 (write-line (concatenate 'string ";@ " line) code-stream))
                ((eq state 'skip)
                 (write-line line code-stream))))))

(defun tangle-file
    (doc-filename
     &optional (code-filename (make-pathname :type "lisp"
                                             :defaults doc-filename)))
  (with-open-file (in doc-filename)
    (with-open-file (out code-filename :direction :output :if-exists :supersede)
      (tangle in out))))

(defun weave (code-stream doc-stream)
  "Takes an input stream containing code with literate comments and an output
   stream to write documentation content to."
  (let ((state 'code))
    (write-line "<!--@lisp -->" doc-stream)
    (loop for line = (read-line code-stream nil)
       while line
       do (cond ((and (eq state 'code) (string= line "#|@"))
                 (setf state 'doc))
                ((and (eq state 'doc) (string= line "@|#"))
                 (setf state 'code))
                ((and (eq state 'code) (string= line ";@"))
                 (write-line "<!--" doc-stream)
                 (setf state 'skip))
                ((and (eq state 'skip) (string= line ";@"))
                 (write-line "-->" doc-stream)
                 (setf state 'code))
                ((eq 0 (search ";@@ " line))
                 (write-line (concatenate 'string "<!--" (subseq line 3) " -->")
                             doc-stream))
                ((eq 0 (search ";@ " line))
                 (write-line (subseq line 3) doc-stream))
                ((eq state 'code)
                 (write-line (concatenate 'string "> " line) doc-stream))
                ((eq state 'doc)
                 (write-line line doc-stream))
                ((eq state 'skip)
                 (write-line line doc-stream))))))

(defun weave-file
    (code-filename
     &optional (doc-filename (make-pathname :type "md"
                                            :defaults code-filename)))
  (with-open-file (in code-filename)
    (with-open-file (out doc-filename :direction :output :if-exists :supersede)
      (weave in out))))

(defun weave-web (web-stream doc-stream path-name)
  (loop for line = (read-line web-stream nil)
       while line
     do (let ((file-name (string-trim '(#\< #\>) line)))
          (if (= 4 (- (length line) (length file-name)))
              (let ((sub-file-name (merge-pathnames file-name path-name)))
                (with-open-file (sub-file sub-file-name)
                  (cond ((string= (pathname-type file-name) "web")
                         (weave-web sub-file doc-stream sub-file-name))
                        ((string= (pathname-type file-name) "lisp")
                         (weave sub-file doc-stream))
                        (t (loop for sub-line = (read-line sub-file nil)
                              while sub-line
                              do (write-line sub-line doc-stream))))))
              (write-line line doc-stream)))))

(defun weave-web-file
    (web-filename
     &optional (doc-filename (make-pathname :type "md"
                                            :defaults web-filename)))
  (with-open-file (in web-filename)
    (with-open-file (out doc-filename :direction :output :if-exists :supersede)
      (weave-web in out web-filename))))

(defun convert-file (in-filename &optional out-filename)
  "Since there is only one valid conversion from any given file, this function
   determines which one it is and performs it. If TANGLE and WEAVE form an
   isomorphism, this is a homomorphism."
  (let ((out-file-type))
    (funcall (cond ((string= (pathname-type in-filename) "web")
                    (setf out-file-type "md")
                    #'weave-web-file)
                   ((string= (pathname-type in-filename) "md")
                    (setf out-file-type "lisp")
                    #'tangle-file)
                   (t (setf out-file-type "md")
                      #'weave-file))
             in-filename
             (or out-filename
                 (make-pathname :type out-file-type :defaults in-filename)))))
