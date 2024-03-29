;@
(in-package #:nclb)
;@

#|@
# Introduction

This is _NCLB_ (No Coder Left Behind), a literate programming system. It is
less like _CWEB_ than anything I've seen. The _CWEB_ manual says “[w]riting
CWEB programs is something like writing TEX documents, but with an additional
‘C mode’ that is added to TEX’s horizontal mode, vertical mode, and math mode.”
In contrast, _NCLB_ removes the idea of a “web” and all files are either valid
documentation files or valid code files, with the operations `tangle` and
`weave` simply converting one to the other. In a code file, the documentation is
contained in specially-marked comments and in a documentation file, the code is
in designated code sections. There are a few reasons for this:

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

<!-- merge this in somewhere earlier -->

Literate programming is useful to people who are reading the code – either
developers working on it or users learning from it. In the former case, less is
better, because those who change the code often ignore the docs while those who
try to understand the system often ignore the code – if the two aren't in sync,
trouble brews.

Documentation for users of a system belong in a combination of docstrings and
literate tutorials & test cases. E.g., this literate document is part of a
tutorial on _NCLB_, not part of the _NCLB_ source code. Tutorials and the like
benefit from literate programming because the code sections can be compiled and
tested and the text sections will be consumed as the primary content and
potentially checked by editors.

Okay, so there _is_ a `tangle`, but it’s not used on the same original file.
`tangle` converts valid documentation to valid code. There is still no “web”
file, there are only code and documentation files. `weave` converts from code to
documentation and `tangle` converts from documentation to code. At least in
their current state, they are not isomorphic, but it does seem desirable that

    tangle(weave(x)) = x

However, there is some normalization that happens, so it is better to expect

    tangle(weave(tangle(x))) = tangle(x)

to compensate for that.
@|#
;@
(defun cat (&rest arguments)
  "Just an abbreviation."
  (apply #'concatenate 'string arguments))

(defstruct (file-definition (:conc-name nil))
  (names)
  (extensions)
  (block-begin)
  (block-end)
  (single-line)
  (single-line-comment)
  (single-line-comment-end "") ; NB: hack for Markdown
  (comment-begin)
  (comment-end))

(defvar *code-file-definitions*
  (list (make-file-definition :names '("c" "c++")
                              :extensions '("c" "cpp" "C")
                              :block-begin "/*"
                              :block-end "*/"
                              :single-line "//")
        (make-file-definition :names '("lisp")
                              :extensions '("lisp")
                              :block-begin "#|"
                              :block-end "|#"
                              :single-line ";")))

(defvar *default-doc-format*
  (make-file-definition :names '("markdown")
                        :extensions '("md" "markdown")
                        :block-begin "```"
                        :block-end "```"
                        :single-line "    "
                        :single-line-comment "<!--"
                        :single-line-comment-end "-->"
                        :comment-begin "<!--"
                        :comment-end "-->"))

(defvar *doc-file-definitions*
  (list *default-doc-format*
        (make-file-definition :names '("latex" "tex")
                              :extensions '("tex")
                              :block-begin "\\begin{verbatim}"
                              :block-end "\\end{verbatim}"
                              :single-line-comment "%"
                              :comment-begin "\\begin{comment}"
                              :comment-end "\\end{comment}")
        (make-file-definition :names '("org")
                              :extensions '("org")
                              :block-begin "#+BEGIN_SRC"
                              :block-end "#+END_SRC"
                              :single-line ": "
                              :single-line-comment "#"
                              :comment-begin "#+BEGIN_COMMENT"
                              :comment-end "#+END_COMMENT")))

(defun find-definition (extension definition-list)
  (find extension definition-list
        :test (rcurry #'member :test #'string=) :key #'extensions))

(defun find-doc-definition (extension)
  (find-definition extension *doc-file-definitions*))

(defun find-code-definition (extension)
  (find-definition extension *code-file-definitions*))

(defun tangle (doc-stream code-stream doc-language &key reversiblep)
  "Takes an input stream containing docs with code sections and an output
   stream to write code to, as well as the struct containing the definitions for
   the doc language used. It returns the code language that was used."
  (let* ((language-line (read-line doc-stream nil))
         (code-language (find (subseq language-line
                                      (1+ (length (comment-begin doc-language)))
                                      (position #\Space language-line))
                              *code-file-definitions*
                              :test (rcurry #'member :test #'string=)
                              :key #'names))
         (state 'doc))
    (unless (eq doc-language *default-doc-format*)
      (write-line (cat (single-line code-language) "@" (first (names doc-language)))
                  code-stream))
    (loop for line = (read-line doc-stream nil)
       while line
       do (cond ((and (eq state 'doc) (string= line (block-begin doc-language)))
                 (setf state 'code))
                ((and (eq state 'code) (string= line (block-end doc-language)))
                 (setf state 'doc))
                ((and (eq state 'doc)
                      (string= line (comment-begin doc-language)))
                 (write-line (cat (single-line code-language) "@")
                             code-stream)
                 (setf state 'skip))
                ((and (eq state 'skip)
                      (string= line (comment-end doc-language)))
                 (write-line (cat (single-line code-language) "@")
                             code-stream)
                 (setf state 'doc))
                ((eq 0 (search (comment-begin doc-language) line))
                 (when reversiblep
                   (write-line (cat (single-line code-language) "@@"
                                    (subseq line
                                            (length (comment-begin doc-language))
                                            (- (length line)
                                               (1+ (length (comment-end
                                                            doc-language))))))
                               code-stream)))
                ;; ((eq 0 (search (single-line doc-language) line))
                ;;  (write-line (subseq line (length (single-line doc-language))
                ;;              code-stream))
                ((eq state 'code)
                 (write-line line code-stream))
                ((eq state 'doc)
                 (when reversiblep
                   (write-line (cat (single-line code-language) "@ " line)
                               code-stream)))
                ((eq state 'skip)
                 (write-line line code-stream))))
    code-language))

(defun tangle-file (doc-filename &key output-filename reversiblep)
  "Takes a documentation file to tangle into code, and optionally a code
   filename to write to (by default it will use the same name as the
   doc-filename with an appropriate file extension for the code language). It
   returns the truename of the file written to."
  (with-open-file (in doc-filename)
    (with-open-file (out (or output-filename
                             (make-pathname :type "nclb" :defaults doc-filename))
                         :direction :output :if-exists :supersede)
      (let ((code-lang (tangle in
                               out
                               (find-doc-definition (pathname-type
                                                     doc-filename))
                               :reversiblep reversiblep)))
        (unless output-filename
          (rename-file out
                       (make-pathname :type (first (extensions code-lang))
                                      :defaults doc-filename)
                       :if-exists :supersede)))
      (truename out))))

(defun weave (code-stream doc-stream code-language &key reversiblep)
  "Takes an input stream containing code with literate comments and an output
   stream to write documentation content to as well as the struct containing the
   definitions for the code language used. It returns the doc language that was
   used."
  (let ((doc-language *default-doc-format*)
        (state 'code)
        (writtenp nil))
    (flet ((%write-line (string stream)
             (when (and (not writtenp) reversiblep)
               (setf writtenp t)
               (write-line (cat (single-line-comment doc-language) "@"
                                (first (names code-language))
                                " " (single-line-comment-end doc-language))
                           doc-stream))
             (write-line string stream)))
      (loop for line = (read-line code-stream nil)
         while line
         do (cond ((and (eq state 'code)
                        (string= line (cat (block-begin code-language) "@")))
                   (setf state 'doc))
                  ((and (eq state 'doc)
                        (string= line (cat "@" (block-end code-language))))
                   (setf state 'code))
                  ((and (eq state 'code)
                        (string= line (cat (single-line code-language) "@")))
                   (when reversiblep
                     (%write-line (comment-begin doc-language) doc-stream))
                   (setf state 'skip))
                  ((and (eq state 'skip)
                        (string= line (cat (single-line code-language) "@")))
                   (when reversiblep
                     (%write-line (comment-end doc-language) doc-stream))
                   (setf state 'code))
                  ((eq 0 (search (cat (single-line code-language) "@@ ") line))
                   (when reversiblep
                     (%write-line (cat (comment-begin doc-language)
                                       (subseq line
                                               (+ 3
                                                  (length (single-line code-language))))
                                       " " (comment-end doc-language))
                                 doc-stream)))
                  ((eq 0 (search (cat (single-line code-language) "@ ") line))
                   (%write-line (subseq line
                                        (+ 2
                                           (length (single-line code-language))))
                                doc-stream))
                  ((eq 0 (search (cat (single-line code-language) "@") line))
                   (setf doc-language
                         (find (subseq line
                                       (+ 1 (length (single-line code-language))))
                               *doc-file-definitions*
                               :test (rcurry #'member :test #'string=)
                               :key #'names)))
                  ((eq state 'code)
                   (%write-line (cat (single-line doc-language) line)
                                doc-stream))
                  ((eq state 'doc)
                   (%write-line line doc-stream))
                  ((eq state 'skip)
                   (when reversiblep
                     (%write-line line doc-stream))))))
    doc-language))

(defun weave-file (code-filename &key output-filename reversiblep)
  "Takes a code file to weave into docs, and optionally a doc
   filename to write to (by default it will use the same name as the
   code-filename with an appropriate file extension for the doc language). It
   returns the truename of the file written to."
  (with-open-file (in code-filename)
    (with-open-file (out (or output-filename
                             (make-pathname :type "nclb"
                                            :defaults code-filename))
                         :direction :output :if-exists :supersede)
      (let ((doc-lang (weave in
                             out
                             (find-code-definition (pathname-type
                                                    code-filename))
                             :reversiblep reversiblep)))
        (unless output-filename
          (rename-file out
                       (make-pathname :type (first (extensions doc-lang))
                                      :defaults code-filename)
                       :if-exists :supersede)))
      (truename out))))

(defun weave-web (web-stream doc-stream path-name &key reversiblep)
  "Takes a stream containing web-formatted docs and a stream to write processed
   docs to, as well as the base path name to use for all relative filenames in
   the web-stream. Returns the document language used."
  (let ((doc-lang))
    (loop for line = (read-line web-stream nil)
       while line
       do (let ((file-name (string-trim '(#\< #\>) line)))
            (if (= 4 (- (length line) (length file-name)))
                (let ((sub-file-name (merge-pathnames file-name path-name)))
                  (with-open-file (sub-file sub-file-name)
                    (cond ((string= (pathname-type file-name) "web")
                           (weave-web sub-file doc-stream sub-file-name))
                          ((find-code-definition (pathname-type file-name))
                           (let ((new-lang (weave sub-file
                                                  doc-stream
                                                  (find-code-definition
                                                   (pathname-type sub-file-name))
                                                  :reversiblep reversiblep)))
                             (if doc-lang
                                 (unless (eq new-lang doc-lang)
                                   (error "~A can not be woven into ~A"
                                          sub-file path-name))
                                 (setf doc-lang new-lang))))
                          (t (let ((new-lang (find-doc-definition (pathname-type sub-file-name))))
                               (if doc-lang
                                   (unless (eq new-lang doc-lang)
                                     (error "~A can not be woven into ~A"
                                            sub-file path-name))
                                   (setf doc-lang new-lang)))
                             (loop for sub-line = (read-line sub-file nil)
                                while sub-line
                                do (write-line sub-line doc-stream))))))
                (write-line line doc-stream))))
    doc-lang))

(defun weave-web-file (web-filename &key output-filename reversiblep)
  "Takes a web file to weave into docs, and optionally a doc
   filename to write to (by default it will use the same name as the
   web-filename with an appropriate file extension for the doc language). It
   returns the truename of the file written to."
  (with-open-file (in web-filename)
    (with-open-file (out (or output-filename
                             (make-pathname :type "nclb" :defaults web-filename))
                         :direction :output :if-exists :supersede)
      (let ((doc-lang (weave-web in out web-filename :reversiblep reversiblep)))
        (unless output-filename
          (rename-file out
                       (make-pathname :type (first (extensions doc-lang))
                                      :defaults web-filename)
                       :if-exists :supersede)))
      (truename out))))

(defun convert-file (input-filename &key output-filename reversiblep)
  "Since there is only one valid conversion from any given file, this function
   determines which one it is and performs it. If TANGLE and WEAVE form an
   isomorphism, this is a homomorphism."
  (funcall (cond ((string= (pathname-type input-filename) "web")
                  #'weave-web-file)
                 ((find-doc-definition (pathname-type input-filename))
                  #'tangle-file)
                 ((find-code-definition (pathname-type input-filename))
                  #'weave-file)
                 (t (error "unknown file extension: ~A"
                           (pathname-type input-filename))))
           input-filename
           :output-filename output-filename
           :reversiblep reversiblep))
;@
