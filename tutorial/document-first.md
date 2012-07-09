While I recommend writing your literate files in a [code-first style](code-first.md), it is just as easy to write in a document-first style, and I’ll explain that here.

Currently, each file must begin with a declaration indicating which programming language to generate source for.

> &lt;!--@c --&gt;

**Note**: I am using Markdown (HTML) comments here, because they are commonly understood. Replace `&lt;!--` and `--&gt;` with your documentation system’s comment block delimiters. And replace `\`\`\`` with your documentation system’s code block delimiters.

Single-line code blocks (indicated by `> ` in Markdown) will be maintained as part of the commented documentation when “tangled” to code, while code blocks will be converted to active code.

> > this = descriptive_code;
>
> ```
> however_this = is_actual_code;
> ```

When that is `tangled`, it becomes

> /*@
> > this = descriptive_code;
> @*/
> 
> however_this = is_actual_code;

Code that should be hidden in the documentation, but visible in the source is marked up as

> &lt;!--@
> #define CRAZY_MACRO = THAT_NO_ONE_WANTS_TO_SEE
> @--&gt;
