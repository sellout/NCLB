# What is NCLB?

NCLB (No Coder Left Behind) is a
[literate programming](http://en.wikipedia.org/wiki/Literate_programming) system. Its goal is to make writing documents (books, tutorials) that contain testable code samples much easier as well as to enrich general comments and documentation embedded in other source files.

Literate programming produces _developer_ documentation as opposed to user documentation. It is not meant to replace user documentation such as docstrings or JavaDoc, but rather to help the developers who need to understand or modify the implementation itself (as well as tutuorial readers who also need to understand the code).

# How does NCLB compare to CWEB (and similar literate systems)?

One of the problems with the *WEB class of literate systems is that there is both a
high barrier to use and a high barrier to sharing. With *WEB systems, you write a
`.web` file that wraps both code and documentation blocks in a third syntax. This
means that before you can compile any code, you need to acquire, build and process
your files with the literate system. They also use (La)Teχ exclusively as a
documentation syntax – while this can be very attractive when made into a book, it is
often hard to read/edit the source, and overkill (or even counterproductive) when
generating documents for the Web. Both of these issues affect both the developer and
any users who might build or patch the system themselves.

These shortcomings mean that literate programming is only useful when the benefits are
high enough to overcome those hurdles – books and tutorials. And that in itself isn't
so bad – the benefits in most source code is minimal, but since developers don't have
any familiarity with the tools before embarking on such a document-heavy project, they
often don't even come to mind, or if they do, the added complexity makes them drop it
in short order. NCLB removes these hurdles.

Almost all NCLB documents can be written as either valid code files or valid
documentation files, and freely converted between the two, with no files that can only
be processed by the literate system. This means that neither the developer nor the
users need to install the literate system at all – if you write in code-form, then
everything will compilpe properly without pre-processing.

Also, while it is possible to use NCLB with LaTeχ, by default it processes
documentation as [Markdown](http://en.wikipedia.org/wiki/Markdown). This is both more
readable in source form, and simpler.

# How do I use NCLB?

The easiest way is to not download anything at all and just start writing your
comments slightly differently. Markdown’s syntax is very easy to pick up. If you look
at the source of this file, you’ll see Markdown everywhere. There are two main options
for writing NCLB-friendly files. With a [code-first style](code-first.md) (which I
prefer) you write valid source code files that can be processed into documentation,
and with a [document-first style](document-first.md) you write documentation files
that can be processed into source code.
