I prefer a code-first style – my documents are all code files with special
comments. This allows me to quickly compile and/or load the code without any
processing step, which is especially helpful to users who may not have NCLB
installed.

To indicate that you’re writing a comment that you want processed by NCLB,
just attach an `@` to the delimiters (and put them on their own lines):

    /*@
    ## Section Header
    The functions in this section do some stuff.
    @*/
    
    int main(int, char**) {
      //@ A single line of documentation in my function.
      // Regular comment on the code.
      do_stuff();
    }

**Note**: I’m using C syntax here because I feel like it’s something that most
people are familiar with. Just replace `/*` and `*/` with your languages block
comment delimiters and `//` with your language’s single-line comment
delimiter.

That’s it – your comments are just as readable as before and now they have the
added bonus of being able to be processed into a pretty document.

The above section, when “woven” into Markdown, ends up looking like:

    ## Section Header
    The functions in this section do some stuff.
    
    \`\`\`c
    int main(int, char**) {
    \`\`\`
    A single line of documentation in my function.
    \`\`\`c
      // Regular comment on the code.
      do_stuff();
    }
    \`\`\`

And when “tangled” back into C, it should look (pretty close to) the original
section.

**Note**: `tangle` and `weave` are the traditional names of the operations for
converting a web file to code and to documentation, respectively. In our case,
they convert directly between code and documentation, without the extra web
file.

There are some additional bits of markup that you might also use. `//@` on a
line by itself is a boundary for hidden code. Place this before and after a
block of code that you don’t want in the documentation.

    //@
    #define SOME_MACRO = WHOSE_DEFINITION_IS_MORE_NOISY_THAN_USEFUL
    //@

will become

    &lt;!--@ #define SOME_MACRO = WHOSE_DEFINITION_IS_MORE_NOISY_THAN_USEFUL --&gt;

in the generated Markdown. (It isn’t eliminated completely, because we want to
be able to get back the code from the Markdown file.)

There is also `//@@` and `/*@@ ... @@*/` for comments that should be hidden in
the documentation.

    //@@ OMG, I can’t believe my boss wants me to write this.

This is basically a shorter way to write
    
    //@
    // OMG, I can’t believe my boss wants me to write this.
    //@

although the comment generated for the documentation will be slightly
different.

Finally, if you want to use a format other than Markdown for the
documentation, add

    /*@latex */

to the top of your file, and `weave` will produce LaTeχ rather than Markdown.
