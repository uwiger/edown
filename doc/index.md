

#The edown application#
The edown application
=====================

Status:
------
More-or-less readable Markdown can be generated.
Copyright © 2010 Erlang Solutions Ltd

__Authors:__ [`ulf.wiger@erlang-solutions.com`](mailto:ulf.wiger@erlang-solutions.com).

Status:
------
More-or-less readable Markdown can be generated.
A doclet needs to be written that also creates 
a markdown-based index and overview. Currently, the 
edoc_doclet creates an index.html and overview.html,
which do not point to the .md files.


To generate markdown edoc, run:


<pre>
edoc:application(App, [{doclet, edown_doclet} | OtherOpts]).
</pre>



The `edown_xmerl` module is used as an xmerl export module.
It converts xmerl's "simple xml" to Markdown syntax. Note that
GH-flavored Markdown allows HTML markup (at least common tags),
but doesn't expand markdown markup inside HTML markup, so the 
`edown_xmerl` module has to know the context in which it operates.


NOTE
====
EDoc provides a plugin structure, so that one may specify own 
layout modules, export modules, and doclets. However, there is 
some overlap esp. between the layout and doclet modules, and 
several functions are expected to produce files on their own.
This causes a problem for EDown, since it cannot handle frames.
Instead, it would probably like to create one overview file with
different sections. It would have been better to have a framework
where some plugin functions identify the different files to be 
written, and the outline of each, other plugins convert to suitable
content representation (e.g. HTML or Markdown), and EDoc then 
writes the files necessary.

For now, EDown focuses on producing reasonable Markdown, rather
than complying fully with the plugin framework. That is, the 
edown_doclet module will not go out of its way to function together
with any other layout module than edown_layout, and vice versa.

##Modules##

<table width="100%" border="0" summary="list of modules">
<tr><td><a href="edown_doclet.md" class="module">edown_doclet</a></td></tr>
<tr><td><a href="edown_layout.md" class="module">edown_layout</a></td></tr>
<tr><td><a href="edown_lib.md" class="module">edown_lib</a></td></tr>
<tr><td><a href="edown_xmerl.md" class="module">edown_xmerl</a></td></tr></table>
