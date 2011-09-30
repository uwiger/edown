

#Module edown_layout#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Markdown layout module for EDoc.



Copyright (c) 2010 Erlang Solutions Ltd

__Authors:__ Ulf Wiger ([`ulf.wiger@erlang-solutions.com`](mailto:ulf.wiger@erlang-solutions.com)).

<h2><a name="description">Description</a></h2>

  Derived from `edoc_layout`, which is part of the Erlang/OTP application EDoc.
The module is intended to be used together with edoc.

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#markdown-3">markdown/3</a></td><td></td></tr><tr><td valign="top"><a href="#module-2">module/2</a></td><td>The layout function.</td></tr><tr><td valign="top"><a href="#overview-2">overview/2</a></td><td></td></tr><tr><td valign="top"><a href="#package-2">package/2</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="markdown-3"></a>

<h3>markdown/3</h3>





`markdown(Title, CSS, Body) -> any()`

<a name="module-2"></a>

<h3>module/2</h3>





`module(Element, Options) -> any()`





The layout function.

Options to the standard layout:



<dt><code>{index_columns, integer()}</code>
</dt>




<dd>Specifies the number of column pairs used for the function
index tables. The default value is 1.
</dd>




<dt><code>{pretty_printer, atom()}</code>
</dt>




<dd>Specifies how types and specifications are pretty printed.
If the value <code>erl_pp</code> is specified the Erlang pretty printer
(the module <code>erl_pp</code>) will be used. The default is to do
no pretty printing which implies that lines can be very long.
</dd>




<dt><code>{stylesheet, string()}</code>
</dt>




<dd>Specifies the URI used for referencing the stylesheet. The
default value is <code>"stylesheet.css"</code>. If an empty string is
specified, no stylesheet reference will be generated.
</dd>




<dt><code>{sort_functions, boolean()}</code>
</dt>




<dd>If <code>true</code>, the detailed function descriptions are listed by
name, otherwise they are listed in the order of occurrence in
the source file. The default value is <code>true</code>.
</dd>




<dt><code>{xml_export, Module::atom()}</code>
</dt>




<dd>Specifies an <a href="http://www.erlang.org/doc/man/index.html" target="_top"><code>xmerl</code></a> callback module to be
used for exporting the documentation. See <a href="http://www.erlang.org/doc/man/xmerl.html#export_simple_content-2"><code>//xmerl/xmerl:export_simple_content/2</code></a> for details.
</dd>





__See also:__ [//edoc/edoc:layout/2](http://www.erlang.org/doc/man/edoc.html#layout-2), [edown_doclet:layout/2](edown_doclet.md#layout-2).<a name="overview-2"></a>

<h3>overview/2</h3>





`overview(E, Options) -> any()`

<a name="package-2"></a>

<h3>package/2</h3>





`package(E, Options) -> any()`

<a name="type-1"></a>

<h3>type/1</h3>





`type(E) -> any()`

