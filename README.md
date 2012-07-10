<!-- -*- mode: markdown -*- -->

    
# Introduction

This is _NCLB_ (No Coder Left Behind), a literate programming system. It is
less like _CWEB_ than anything I've seen. The _CWEB_ manual says “[w]riting
CWEB programs is something like writing TEX documents, but with an additional
‘C mode’ that is added to TEX’s horizontal mode, vertical mode, and math mode.”
In contrast, _NCLB_ removes the idea of a “web” and all files are either valid
documentation files or valid code files, with the operations `tangle` and `weave`
simply converting one to the other. In a code file, the documentation is contained in
specially-marked comments and in a documentation file, the code is in designated code
sections. There are a few reasons for this:

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

Literate programming is useful to people who are reading the code – either developers
working on it or users learning from it. In the former case, less is better, because
those who change the code often ignore the docs while those who try to understand the
system often ignore the code – if the two aren't in sync, trouble brews.

Documentation for users of a system belong in a combination of docstrings and literate
tutorials & test cases. E.g., this literate document is part of a tutorial on _NCLB_,
not part of the _NCLB_ source code. Tutorials and the like benefit from literate
programming because the code sections can be compiled and tested and the text sections
will be consumed as the primary content and potentially checked by editors.

Okay, so there _is_ a `tangle`, but it’s not used on the same original file. `tangle`
converts valid documentation to valid code. There is still no “web” file, there are
only code and documentation files. `weave` converts from code to documentation and
`tangle` converts from documentation to code. At least in their current state, they
are not isomorphic, but it does seem desirable that

    tangle(weave(x)) = x

However, there is some normalization that happens, so it is better to expect that

    tangle(weave(tangle(x))) = tangle(x)

to compensate for that.

I lied about there being no “web” file, but you use them very rarely. They are used for document files that aggregate other files in cases where the document format doesn’t support inlining subdocuments. IE, Markdown needs a web file for aggregating, but LaTeχ does not.

The syntax is very simple – it uses the same formatting as the subdocuments, with an added `<<foo/bar.md>>` syntax to include a file at a particular location. With LaTeχ, you would use `\input{foo/bar.tex}` or `\include{foo/bar.tex}` as appropriate, so no web file is necessary there. Even in formats like Markdown, I would recommend using links instead of embedding when possible.

In fact, this file is generated from a web file, so it can pull in documentation from across the system to make a comprehensive README.

Please also check out [the tutorial](tutorial/tutorial.md).
