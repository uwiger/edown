markedoc 0.1
============

 **markedoc** helps you keep your project's README.md in sync with your
 overview.edoc.

Status: alpha. Many things work. Some others do not. See [Status][].

markedoc translates [Markdown][] formatted texts into [Erlang][] [EDoc][]
format, for inclusion into [EDoc][] generated html docs.

The actual script file is in the bin folder: bin/markedoc.sed.

markedoc is but a brief [sed][] command file to convert markdown to edoc. Use it
to translate your project's README.md into a README.edoc to include in your
Erlang project's main overview.edoc file.

markedoc is part of **[edown][]**.

You contribution to make markedoc better is highly welcome.

Use
---
At the command line:

	$ sed -E -f markedoc.sed <markdown file> > <edoc file>

Requirements
------------
* **[sed]**: is part of any Linux and Mac OSX distribution. You could get it for [Windows, too][winsed].

* **[Erlang/OTP][Erlang]**: see below.  

Sample
------

For your own projects you'd copy markedoc.sed in the right place and do something like:

	$ echo "@doc " > doc/README.edoc
	$ sed -E -f bin/markedoc.sed README.md >> doc/README.edoc
	$ erl -noshell -run edoc_run application "'myapp'" '"."' '[{def,{vsn,""}}]'	

And that's it. This could also be part of your Makefile. This way, the
README.edoc becomes part of your generated EDoc html pages, you would use a
@docfile tag in your overview.edoc file, like so:

	@docfile "doc/README.edoc"

Status
------

 **Alpha**. You can do nice things but it likes to trip up EDoc, which is kind of easy to do.

There are  many ways to create formats that will make the EDoc generator tilt and unfortunately, the errors it throws are sometimes not quite so illuminating to the reader. But why not try an incremental approach and see what works. As you can see from the [live sample][sample], it's quite a lot that *does* work and some bits can be worked out. Please experiment and push your fixes.

 **Thanks!**

Notes
-----

 **[Erlang][]** is a programming language used to build massively scalable soft real-time systems with requirements on high availability. Some of its uses are in telecom, banking, e-commerce, computer telephony and instant messaging. Erlang's runtime system has built-in support for concurrency, distribution and fault tolerance. Erlang comes bundled with the Open Telecom Platform, OTP.

[Erlang]: http://www.erlang.org/doc/  

 **[EDoc][]** is the Erlang program documentation generator. Inspired by the Javadoc tool for the Java programming language, EDoc is adapted to the conventions of the Erlang world, and has several features not found in Javadoc. Edoc is part of the Erlang/OTP distribution.

[EDoc]: http://www.erlang.org/doc/apps/edoc/chapter.html  

 **[edown][]** is an EDoc extension for generating Github-flavored Markdown. It uses edoc-style commented Erlang sources to create markdown files from them. 

[edown]: https://github.com/esl/edown  

 **[Markdown][]** is a text-to-HTML conversion tool for web writers. Markdown allows you to write using an easy-to-read, easy-to-write plain text format, then convert it to structurally valid XHTML (or HTML).

[Markdown]: http://daringfireball.net/projects/markdown/  

 **[sed][]** ('stream editor') is a Unix utility that parses text files and implements a programming language which can apply textual transformations to such files. It reads input files line by line (sequentially), applying the operation which has been specified via the command line (or a sed script), and then outputs the line. It is available today for most operating systems. There seems to be [one for Windows][winsed], too.

[sed]: http://en.wikipedia.org/wiki/Sed  

[winsed]: http://gnuwin32.sourceforge.net/packages/sed.htm  

[sample]: https://github.com/Eonblast/Emysql/raw/master/README.md "This markdown file is translated alright by markedoc."  


Caveats / Todos
---------------
* Underlined ("==="/"---") headlines currently don't work, use the '#' variant instead  
* **'[1]: ...'-style end note references need two spaces at the end of the line**    
* add two new lines at end of your markdown file to avoid loosing the last line.  
* Local anchor jumps fail  


Not So Bad Todos
----------------
* robust alternates not tested for some time  
* space before javascript links should go  
* protect ampersands  


License
-------
This script is free software. It comes without any warranty.


Author
------
H. Diedrich <hd2010@eonblast.com>


History
-------
01/31/11 - 0.1 - **first release**


