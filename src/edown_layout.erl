%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
%% @author Ulf Wiger <ulf.wiger@erlang-solutions.com>
%% @copyright 2010 Erlang Solutions Ltd 
%% @end
%% =====================================================================

%% @doc Markdown layout module for EDoc.
%% Derived from `edoc_layout', which is part of the Erlang/OTP application EDoc.
%% The module is intended to be used together with edoc.
%% @end

-module(edown_layout).

-export([module/2, package/2, overview/2, type/1]).
-export([markdown/3]).

-import(edoc_report, [report/2]).

-include_lib("xmerl/include/xmerl.hrl").

-define(HTML_EXPORT, edown_xmerl).
-define(DEFAULT_XML_EXPORT, ?HTML_EXPORT).
-define(OVERVIEW_SUMMARY, "overview-summary.html").
-define(STYLESHEET, "stylesheet.css").
-define(NL, "\n").
-define(DESCRIPTION_TITLE, "Description").
-define(DESCRIPTION_LABEL, "description").
-define(DATA_TYPES_TITLE, "Data Types").
-define(DATA_TYPES_LABEL, "types").
-define(FUNCTION_INDEX_TITLE, "Function Index").
-define(FUNCTION_INDEX_LABEL, "index").
-define(FUNCTIONS_TITLE, "Function Details").
-define(FUNCTIONS_LABEL, "functions").


%% @doc The layout function.
%%
%% Options to the standard layout:
%% <dl>
%%  <dt>{@type {index_columns, integer()@}}
%%  </dt>
%%  <dd>Specifies the number of column pairs used for the function
%%      index tables. The default value is 1.
%%  </dd>
%%  <dt>{@type {stylesheet, string()@}}
%%  </dt>
%%  <dd>Specifies the URI used for referencing the stylesheet. The
%%      default value is `"stylesheet.css"'. If an empty string is
%%      specified, no stylesheet reference will be generated.
%%  </dd>
%%  <dt>{@type {sort_functions, bool()@}}
%%  </dt>
%%  <dd>If `true', the detailed function descriptions are listed by
%%      name, otherwise they are listed in the order of occurrence in
%%      the source file. The default value is `true'.
%%  </dd>
%%  <dt>{@type {xml_export, Module::atom()@}}
%%  </dt>
%%  <dd>Specifies an {@link //xmerl. `xmerl'} callback module to be
%%      used for exporting the documentation. See {@link
%%      //xmerl/xmerl:export_simple_content/2} for details.
%%  </dd>
%% </dl>
%%
%% @see edown_doclet:layout/2
%% @see //edoc/edoc:layout/2

%% NEW-OPTIONS: xml_export, index_columns, stylesheet

module(Element, Options) ->
    XML = layout_module(Element, init_opts(Element, Options)),
    Export = proplists:get_value(xml_export, Options,
				 ?DEFAULT_XML_EXPORT),
    xmerl:export_simple(XML, Export, []).

% Put layout options in a data structure for easier access.

%% %Commented out until it can be made private
%% %@type opts() = #opts{root = string(),
%% %                     stylesheet = string(),
%% %                     index_columns = integer()}

-record(opts, {root, stylesheet, index_columns, sort_functions}).

init_opts(Element, Options) ->
    R = #opts{root = get_attrval(root, Element),
	      index_columns = proplists:get_value(index_columns,
						  Options, 1),
	      sort_functions = proplists:get_value(sort_functions,
						   Options, true)
	     },
    case proplists:get_value(stylesheet, Options) of
	undefined ->
	    S = edoc_lib:join_uri(R#opts.root, ?STYLESHEET),
	    R#opts{stylesheet = S};
	"" ->
	    R;  % don't use any stylesheet
	S when is_list(S) ->
	    R#opts{stylesheet = S}; 
	_ ->
	    report("bad value for option `stylesheet'.", []),
	    exit(error)
    end.


%% =====================================================================
%% XML-BASED LAYOUT ENGINE
%% =====================================================================

%% We assume that we have expanded XML data.

%% <!ELEMENT module (behaviour*, description?, author*, copyright?,
%%                   version?, since?, deprecated?, see*, reference*,
%%                   todo?, typedecls?, functions)>
%% <!ATTLIST module
%%   name CDATA #REQUIRED
%%   private NMTOKEN(yes | no) #IMPLIED
%%   root CDATA #IMPLIED>
%% <!ELEMENT behaviour (#PCDATA)>
%% <!ATTLIST behaviour
%%   href CDATA #IMPLIED>
%% <!ELEMENT description (briefDescription, fullDescription?)>
%% <!ELEMENT briefDescription (#PCDATA)>
%% <!ELEMENT fullDescription (#PCDATA)>
%% <!ELEMENT author EMPTY>
%% <!ATTLIST author
%%   name CDATA #REQUIRED
%%   email CDATA #IMPLIED
%%   website CDATA #IMPLIED>
%% <!ELEMENT version (#PCDATA)>
%% <!ELEMENT since (#PCDATA)>
%% <!ELEMENT copyright (#PCDATA)>
%% <!ELEMENT deprecated (description)>
%% <!ELEMENT see (#PCDATA)>
%% <!ATTLIST see
%%   name CDATA #REQUIRED
%%   href CDATA #IMPLIED>
%% <!ELEMENT reference (#PCDATA)>
%% <!ELEMENT todo (#PCDATA)>
%% <!ELEMENT typedecls (typedecl+)>
%% <!ELEMENT functions (function+)>

%% TODO: improve layout of parameterized modules

layout_module(#xmlElement{name = module, content = Es}=E, Opts) ->
    Args = module_params(get_content(args, Es)),
    Name = get_attrval(name, E),
    Title = case get_elem(args, Es) of
		[] -> ["Module ", Name];
		_ -> ["Abstract module ", Name, " [", {Args}, "]"]
	    end,
    Desc = get_content(description, Es),
    FullDesc = get_content(fullDescription, Desc),
    {ShortDesc, RestDesc} = get_first_sentence(FullDesc),
    Functions = [{function_name(Ex), Ex} || Ex <- get_content(functions, Es)],
    Types = [{type_name(Ex), Ex} || Ex <- get_content(typedecls, Es)],
    SortedFs = lists:sort(Functions),
    Body = ([]   % navigation("top")
            ++ [{h1, Title}]
	    ++ doc_index(FullDesc, Functions, Types)
	    ++ [{p,[]}]
	    ++ ShortDesc
	    ++ [{p,[]}]
	    ++ copyright(Es)
	    ++ deprecated(Es, "module")
	    ++ version(Es)
	    ++ since(Es)
	    ++ behaviours(Es, Name)
	    ++ authors(Es)
	    ++ references(Es)
	    ++ sees(Es)
	    ++ todos(Es)
	    ++ if RestDesc == [] -> [];
		  true -> [{h2, [{a, [{name, "description"}],
				  ["Description"]}]}
			   | RestDesc]
	       end
	    ++ types(lists:sort(Types))
	    ++ function_index(SortedFs, Opts#opts.index_columns)
	    ++ if Opts#opts.sort_functions -> functions(SortedFs);
		  true -> functions(Functions)
	       end
	    %% ++ navigation("bottom")
	    ++ timestamp()),
    %% if Name == "edown_doclet" ->
    %% 	    io:fwrite("edown_doclet:~n"
    %% 		      "-----------------~n"
    %% 		      "~p~n"
    %% 		      "-----------------~n", [Body]);
    %%    true ->
    %% 	    io:fwrite("not edown_doclet (~p)~n", [Name])
    %% end,
    %% xhtml(Title, stylesheet(Opts), Body).
    Res = to_simple(markdown(Title, stylesheet(Opts), Body)),
    %% io:fwrite("Res = ~p~n", [Res]),
    Res.

%% This function is a workaround for a bug in xmerl_lib:expand_content/1 that 
%% causes it to lose track of the parents if #xmlElement{} records are 
%% encountered in the structure.
%%
to_simple([#xmlElement{name = Name, attributes = Attrs, content = Content}|T]) ->
    [{Name, to_simple_attrs(Attrs), to_simple(Content)} | to_simple(T)];
to_simple([#xmlText{} = H | T]) ->
    [H | to_simple(T)];
to_simple([{N,C} | T]) ->
    [{N, to_simple(C)} | to_simple(T)];
to_simple([{N,As,C} | T]) ->
    [{N, As, to_simple(C)} | to_simple(T)];
to_simple([[_|_] = L | T]) ->
    [to_simple(lists:flatten(L)) | to_simple(T)];
to_simple([H|T]) ->
    [H | to_simple(T)];
to_simple([]) ->
    [].

to_simple_attrs(As) ->
    [{K,V} || #xmlAttribute{name = K, value = V} <- As].

module_params(Es) ->
    As = [{get_text(argName, Es1),
	   get_content(fullDescription, get_content(description, Es1))}
	  || #xmlElement{content = Es1} <- Es],
    case As of
	[] -> [];
	[First | Rest] ->
	    [element(1, First) | [ {[", ",A]} || {A, _D} <- Rest]]
    end.

timestamp() ->
    [{p, [{i, [io_lib:fwrite("Generated by EDoc, ~s, ~s.",
			     [edoc_lib:datestr(date()),
			      edoc_lib:timestr(time())])
	      ]}]}].
 
stylesheet(Opts) ->
    case Opts#opts.stylesheet of
	undefined ->
	    [];
	CSS ->
	    [{link, [{rel, "stylesheet"},
		     {type, "text/css"},
		     {href, CSS},
		     {title, "EDoc"}], []}]
    end.

%% navigation(Where) ->
%%     [{p, []},
%%      {'div', [{class, "navbar"}],
%%       [{a, [{name, "#navbar_" ++ Where}], []},
%%        {table, [{width, "100%"}, {border,0},
%% 		{cellspacing, 0}, {cellpadding, 2},
%% 		{summary, "navigation bar"}],
%% 	[{tr,
%% 	  [{td, [{a, [{href, ?OVERVIEW_SUMMARY}, {target,"overviewFrame"}],
%% 		  ["Overview"]}]},
%% 	   {td, [{a, [{href, "http://www.erlang.org/"}],
%% 		  [{img, [{src, "erlang.png"}, {align, "right"},
%% 			  {border, 0}, {alt, "erlang logo"}],
%% 		    []}]}
%% 		]}
%% 	  ]}
%% 	]}
%%       ]}
%%     ].

doc_index(FullDesc, Functions, Types) ->
    case doc_index_rows(FullDesc, Functions, Types) of
	[] -> [];
	Rs ->
	    [{ul, [{class, "index"}],
	      [{li, [{a, [{href, local_label(R)}], [T]}]}
	       || {T, R} <- Rs]}]
    end.

doc_index_rows(FullDesc, Functions, Types) ->
    (if FullDesc == [] -> [];
	true -> [{?DESCRIPTION_TITLE, ?DESCRIPTION_LABEL}]
     end
     ++ if Types == [] -> [];
	   true -> [{?DATA_TYPES_TITLE, ?DATA_TYPES_LABEL}]
	end
     ++ if Functions == [] -> [];
	   true -> [{?FUNCTION_INDEX_TITLE, ?FUNCTION_INDEX_LABEL},
		    {?FUNCTIONS_TITLE, ?FUNCTIONS_LABEL}]
	end).

function_index(Fs, Cols) ->
    case function_index_rows(Fs, Cols, []) of
	[] -> [];
	Rows ->
	    [{h2, [{a, [{name, ?FUNCTION_INDEX_LABEL}],
		    [?FUNCTION_INDEX_TITLE]}]},
	     {table, [{width, "100%"}, {border, 1},
		      {cellspacing,0}, {cellpadding,2},
		      {summary, "function index"}],
	      Rows}]
    end.

function_index_rows(Fs, Cols, Title) ->
    Rows = (length(Fs) + (Cols - 1)) div Cols,
    (if Title == [] -> [];
	true -> [{tr, [{th, [{colspan, Cols * 2}, {align, left}],
			[Title]}]}]
     end
     ++ lists:flatmap(fun index_row/1,
		      edoc_lib:transpose(edoc_lib:segment(Fs, Rows)))).

index_row(Fs) ->
    [{tr, lists:flatmap(fun index_col/1, Fs)}].

index_col({Name, F=#xmlElement{content = Es}}) ->
    [{td, [{valign, "top"}],
      label_href(function_header(Name, F, "*"), F)},
     {td, index_desc(Es)}].

index_desc(Es) ->
    Desc = get_content(description, Es),
    (case get_content(deprecated, Es) of
 	 [] -> [];
 	 _ -> ["(", {em, ["Deprecated"]}, ".) "]
     end
     ++ case get_content(briefDescription, Desc) of
	    [] ->
		equiv(Es);    % no description at all if no equiv
	    ShortDesc ->
		ShortDesc
	end).

label_href(Content, F) ->
    case get_attrval(label, F) of
	"" -> Content;
	Ref -> [{a, [{href, local_label(Ref)}], Content}]
    end.

%% <!ELEMENT function (args, typespec?, returns?, throws?, equiv?,
%%                     description?, since?, deprecated?, see*, todo?)>
%% <!ATTLIST function
%%   name CDATA #REQUIRED
%%   arity CDATA #REQUIRED
%%   exported NMTOKEN(yes | no) #REQUIRED
%%   label CDATA #IMPLIED>
%% <!ELEMENT args (arg*)>
%% <!ELEMENT equiv (expr, see?)>
%% <!ELEMENT expr (#PCDATA)>

functions(Fs) ->
    Es = lists:flatmap(fun ({Name, E}) -> function(Name, E) end, Fs),
    if Es == [] -> [];
       true ->
	    [{a, [{name, ?FUNCTIONS_LABEL}], []},
	     {h2, [?FUNCTIONS_TITLE]} | Es]
    end.

function(Name, E=#xmlElement{content = Es}) ->
    FHead = function_header(Name, E, " *"),
    (label_anchor(FHead, E)
     ++ [{h3, [lists:flatten(FHead)]},
	 {p, []},
	 {p,
	  case typespec(get_content(typespec, Es)) of
	      [] ->
		  signature(get_content(args, Es),
			    get_attrval(name, E));
	      Spec -> Spec
	  end},
	 {p, []}]
     ++ case params(get_content(args, Es)) of
	    [] -> [];
	    Ps -> [{p, Ps}]
	end
     ++ case returns(get_content(returns, Es)) of
	    [] -> [];
	    Rs -> [{p, Rs}]
	end
     ++ throws(Es)
     ++ equiv_p(Es)
     ++ deprecated(Es, "function")
     ++ fulldesc(Es)
     ++ since(Es)
     ++ sees(Es)
     ++ todos(Es)).

function_name(E) ->
    atom(get_attrval(name, E)) ++ "/" ++ get_attrval(arity, E).

function_header(Name, E, Private) ->
    case is_exported(E) of
	true -> [Name];
	false -> [Name, Private]
    end.

is_exported(E) ->
    case get_attrval(exported, E) of
 	"yes" -> true;
 	_ -> false
    end.

label_anchor(Content, E) ->
    case get_attrval(label, E) of
	"" -> Content;
	Ref -> [{a, [{name, Ref}], ""}]
    end.

%% <!ELEMENT args (arg*)>
%% <!ELEMENT arg (argName, description?)>
%% <!ELEMENT argName (#PCDATA)>

%% This is currently only done for functions without type spec.

signature(Es, Name) -> 
    [{tt, [Name, "("] ++ seq(fun arg/1, Es) ++ [") -> any()"]}].

arg(#xmlElement{content = Es}) ->
    [get_text(argName, Es)].

%% parameter and return value descriptions (if any)

params(Es) ->
    As = [{get_text(argName, Es1),
	   get_content(fullDescription, get_content(description, Es1))}
	  || #xmlElement{content = Es1} <- Es],
    As1 = [A || A <- As, element(2, A) /= []],
    if As1 == [] ->
	    [];
       true ->
	    [ { [{tt, [A]}, ": "] ++  D ++ [br] }
	      || {A, D} <- As1]
    end.

returns(Es) ->
    case get_content(fullDescription, get_content(description, Es)) of
	[] ->
	    [];
	D ->
	    ["returns: "] ++  D
    end.

%% <!ELEMENT throws (type, localdef*)>

throws(Es) ->
    case get_content(throws, Es) of
	[] -> [];
	Es1 ->
	    [{p, (["throws ", 
		   {'div', [{class,"html"}], {tt, t_utype(get_elem(type, Es1))}}]
		   ++ local_defs(get_elem(localdef, Es1)))}]
    end.

%% <!ELEMENT typespec (erlangName, type, localdef*)>

typespec([]) -> [];
typespec(Es) ->
    [{'div', [{class,"html"}], [{tt, ([t_name(get_elem(erlangName, Es))]
				      ++ t_utype(get_elem(type, Es)))}]}]
     ++ local_defs(get_elem(localdef, Es)).

%% <!ELEMENT typedecl (typedef, description?)>
%% <!ELEMENT typedef (erlangName, argtypes, type?, localdef*)>

types([]) -> [];
types(Ts) ->
    Es = lists:flatmap(fun ({Name, E}) -> typedecl(Name, E) end, Ts),
    [{h2, [{a, [{name, ?DATA_TYPES_LABEL}],
	    [?DATA_TYPES_TITLE]}]} | Es].

typedecl(Name, E=#xmlElement{content = Es}) ->
    Lbl = Name ++ "()",
    (label_anchor(Lbl, E)
     ++ [{h3, [{class, "typedecl"}], [Lbl]}]
     ++ [{'div', [{class,"html"}], typedef(get_content(typedef, Es))}]
     ++ fulldesc(Es)).

type_name(#xmlElement{content = Es}) ->
    t_name(get_elem(erlangName, get_content(typedef, Es))).

typedef(Es) ->
    Name = ([t_name(get_elem(erlangName, Es)), "("]
  	    ++ seq(fun t_utype_elem/1, get_content(argtypes, Es), [")"])),
    (case get_elem(type, Es) of
 	 [] -> [{b, ["abstract datatype"]}, ": ", {tt, Name}];
 	 Type ->
	     [{tt, Name ++ [" = "] ++ t_utype(Type)}]
     end
     ++ local_defs(get_elem(localdef, Es))).

local_defs([]) -> [];
local_defs(Es) ->
    [{ul, [{class, "definitions"}],
      lists:concat([[{li, [{tt, localdef(E)}]}] || E <- Es])}].

localdef(E = #xmlElement{content = Es}) ->
    (case get_elem(typevar, Es) of
	 [] -> 
	     T_abs = t_abstype(get_content(abstype, Es)),
	     label_anchor(T_abs, E) ++ [{tt, T_abs}];
	 [V] ->
	     t_var(V)
     end
     ++ [" = "] ++ t_utype(get_elem(type, Es))).

fulldesc(Es) ->
    case get_content(fullDescription, get_content(description, Es)) of
	[] -> [];
	Desc -> [{p, Desc}]
    end.

sees(Es) ->
    case get_elem(see, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["See also:"]}, " "] ++ seq(fun see/1, Es1, ["."])}]
    end.

see(E=#xmlElement{content = Es}) ->
    see(E, Es).


see(E, Es) ->
    case href(E) of
	[] -> Es;
	Ref ->
	    [{a, Ref, Es}]
    end.

href(E) ->
    case get_attrval(href, E) of
	"" -> [];
	URI ->
	    T = case get_attrval(target, E) of
		    "" -> [];
		    S -> [{target, S}]
		end,
	    [{href, URI} | T]
    end.

equiv_p(Es) ->
    equiv(Es, true).

equiv(Es) ->
    equiv(Es, false).

equiv(Es, P) ->
    case get_content(equiv, Es) of
	[] -> [];
	Es1 ->
	    case get_content(expr, Es1) of
		[] -> [];
		[Expr] ->
		    Expr1 = [{tt, [Expr]}],
		    Expr2 = case get_elem(see, Es1) of
				[] ->
				    Expr1;
				[E=#xmlElement{}] ->
				    see(E, Expr1)
			    end,
		    Txt = ["Equivalent to "] ++ Expr2 ++ ["."],
		    (case P of
			 true -> [{p, Txt}];
			 false -> Txt
		     end)
	    end
    end.

copyright(Es) ->
    case get_content(copyright, Es) of
	[] -> [];
	Es1 ->
	    [{p, ["Copyright \251 " | Es1]}]
    end.

version(Es) ->
    case get_content(version, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["Version:"]}, " " | Es1]}]
    end.

since(Es) ->
    case get_content(since, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["Introduced in:"]}, " " | Es1]}]
    end.

deprecated(Es, S) ->
    Es1 = get_content(description, get_content(deprecated, Es)),
    case get_content(fullDescription, Es1) of
	[] -> [];
	Es2 ->
	    [{p, [{b, ["This " ++ S ++ " is deprecated:"]}, " " | Es2]}]
    end.

behaviours(Es, Name) ->
    (case get_elem(behaviour, Es) of
	 [] -> [];
	 Es1 ->
	     [{p, ([{b, ["Behaviours:"]}, " "]
		   ++ seq(fun behaviour/1, Es1, ["."]))}]
     end
     ++
     case get_content(callbacks, Es) of
	 [] -> [];
	 Es1 ->
	     [{p, ([{b, ["This module defines the ", {tt, [Name]},
			 " behaviour."]},
		    br, " Required callback functions: "]
		   ++ seq(fun callback/1, Es1, ["."]))}]
     end).

behaviour(E=#xmlElement{content = Es}) ->
    see(E, [{tt, Es}]).

callback(E=#xmlElement{}) ->
    Name = get_attrval(name, E),
    Arity = get_attrval(arity, E),
    [{tt, [Name, "/", Arity]}].

authors(Es) ->
    case get_elem(author, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["Authors:"]}, " "] ++ seq(fun author/1, Es1, ["."])}]
    end.

atom(String) ->
    io_lib:write_atom(list_to_atom(String)).

%% <!ATTLIST author
%%   name CDATA #REQUIRED
%%   email CDATA #IMPLIED
%%   website CDATA #IMPLIED>

author(E=#xmlElement{}) ->
    Name = get_attrval(name, E),
    Mail = get_attrval(email, E),
    URI = get_attrval(website, E),
    (if Name == Mail ->
	     [{a, [{href, "mailto:" ++ Mail}],[{tt, [Mail]}]}];
	true ->
	     if Mail == "" -> [Name];
		true -> [Name, " (", {a, [{href, "mailto:" ++ Mail}],
				      [{tt, [Mail]}]}, ")"]
	     end
     end
     ++ if URI == "" ->
		[];
	   true ->
		[" [", {em, ["web site:"]}, " ",
		 {tt, [{a, [{href, URI}, {target, "_top"}], [URI]}]},
		 "]"]
	end).

references(Es) ->
    case get_elem(reference, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["References"]},
		  {ul, [{li, C} || #xmlElement{content = C} <- Es1]}]}]
    end.

todos(Es) ->
    case get_elem(todo, Es) of
	[] -> [];
	Es1 ->
	    Todos = [{li, [{font, [{color,red}], C}]}
		     || #xmlElement{content = C} <- Es1],
	    [{p, [{b, [{font, [{color,red}], ["To do"]}]},
		  {ul, Todos}]}]
    end.

t_name([E]) ->
    N = get_attrval(name, E),
    case get_attrval(module, E) of
	"" -> atom(N);
	M ->
	    S = atom(M) ++ ":" ++ atom(N),
	    case get_attrval(app, E) of
		"" -> S;
		A -> "//" ++ atom(A) ++ "/" ++ S
	    end
    end.

t_utype([E]) ->
    t_utype_elem(E).

t_utype_elem(E=#xmlElement{content = Es}) ->
    case get_attrval(name, E) of
	"" -> t_type(Es);
	Name ->
	    T = t_type(Es),
	    case T of
		[Name] -> T;    % avoid generating "Foo::Foo"
		T -> [Name] ++ ["::"] ++ T
	    end
    end.

t_type([E=#xmlElement{name = typevar}]) ->
    t_var(E);
t_type([E=#xmlElement{name = atom}]) ->
    t_atom(E);
t_type([E=#xmlElement{name = integer}]) ->
    t_integer(E);
t_type([E=#xmlElement{name = float}]) ->
    t_float(E);
t_type([#xmlElement{name = nil}]) ->
    t_nil();
t_type([#xmlElement{name = list, content = Es}]) ->
    t_list(Es);
t_type([#xmlElement{name = tuple, content = Es}]) ->
    t_tuple(Es);
t_type([#xmlElement{name = 'fun', content = Es}]) ->
    t_fun(Es);
t_type([#xmlElement{name = record, content = Es}]) ->
    t_record(Es);
t_type([E = #xmlElement{name = abstype, content = Es}]) ->
    T = t_abstype(Es),
    see(E, T);
t_type([#xmlElement{name = union, content = Es}]) ->
    t_union(Es).

t_var(E) ->
    [get_attrval(name, E)].

t_atom(E) ->
    [get_attrval(value, E)].

t_integer(E) ->
    [get_attrval(value, E)].

t_float(E) ->
    [get_attrval(value, E)].

t_nil() ->
    ["[]"].

t_list(Es) ->
    ["["] ++ t_utype(get_elem(type, Es)) ++ ["]"].

t_tuple(Es) ->
    ["{"] ++ seq(fun t_utype_elem/1, Es, ["}"]).

t_fun(Es) ->
    ["("] ++ seq(fun t_utype_elem/1, get_content(argtypes, Es),
		 [") -> "] ++ t_utype(get_elem(type, Es))).

t_record(Es) ->
    ["#"] ++ t_type(get_elem(atom, Es)) ++ ["{"]
	++ seq(fun t_field/1, get_elem(field, Es), ["}"]).

t_field(#xmlElement{content = Es}) ->
    t_type(get_elem(atom, Es)) ++ [" = "] ++ t_utype(get_elem(type, Es)).

t_abstype(Es) ->
    ([t_name(get_elem(erlangName, Es)), "("]
     ++ seq(fun t_utype_elem/1, get_elem(type, Es), [")"])).

t_union(Es) ->
    seq(fun t_utype_elem/1, Es, " | ", []).

seq(F, Es) ->
    seq(F, Es, []).

seq(F, Es, Tail) ->
    seq(F, Es, ", ", Tail).

seq(F, [E], _Sep, Tail) ->
    F(E) ++ Tail;
seq(F, [E | Es], Sep, Tail) ->
    F(E) ++ [Sep] ++ seq(F, Es, Sep, Tail);
seq(_F, [], _Sep, Tail) ->
    Tail.

get_elem(Name, [#xmlElement{name = Name} = E | Es]) ->
    [E | get_elem(Name, Es)];
get_elem(Name, [_ | Es]) ->
    get_elem(Name, Es);
get_elem(_, []) ->
    [].

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

get_content(Name, Es) ->
    case get_elem(Name, Es) of
	[#xmlElement{content = Es1}] ->
	    Es1;
	[] -> []
    end.

get_text(Name, Es) ->
    case get_content(Name, Es) of
	[#xmlText{value = Text}] ->
	    Text;
	[] -> ""
    end.

local_label(R) ->
    "#" ++ R.


markdown(Title, _CSS, Body) ->
    [{title, [lists:flatten(Title)]}|
     Body].

%% xhtml(Title, CSS, Body) ->
%%     [{html, [?NL,
%% 	    {head, [?NL,
%% 		    {title, Title},
%% 		    ?NL] ++ CSS},
%% 	    ?NL,
%% 	    {body, [{bgcolor, "white"}], Body},
%% 	    ?NL]
%%      },
%%      ?NL].

%% ---------------------------------------------------------------------

type(E) ->
    type(E, []).

type(E, Ds) ->
    xmerl:export_simple_content(t_utype_elem(E) ++ local_defs(Ds),
				?HTML_EXPORT).

package(E=#xmlElement{name = package, content = Es}, Options) ->
    Opts = init_opts(E, Options),
    Name = get_text(packageName, Es),
    Title = ["Package ", Name],
    Desc = get_content(description, Es),
%    ShortDesc = get_content(briefDescription, Desc),
    FullDesc = get_content(fullDescription, Desc),
    Body = ([{h1, [Title]}]
%	    ++ ShortDesc
	    ++ copyright(Es)
	    ++ deprecated(Es, "package")
	    ++ version(Es)
	    ++ since(Es)
	    ++ authors(Es)
	    ++ references(Es)
	    ++ sees(Es)
	    ++ todos(Es)
	    ++ FullDesc),
    %% XML = xhtml(Title, stylesheet(Opts), Body),
    XML = markdown(Title, stylesheet(Opts), Body),
    xmerl:export_simple_content(XML, ?HTML_EXPORT).

overview(E=#xmlElement{name = overview, content = Es}, Options) ->
    Opts = init_opts(E, Options),
    Title = [get_text(title, Es)],
    Desc = get_content(description, Es),
    ShortDesc = get_content(briefDescription, Desc),
    FullDesc = get_content(fullDescription, Desc),
    Body = ([]
	    %% ++ [{h1, [Title]}]
	    ++ ShortDesc
	    ++ copyright(Es)
	    ++ version(Es)
	    ++ since(Es)
	    ++ authors(Es)
	    ++ references(Es)
	    ++ sees(Es)
	    ++ todos(Es)
	    ++ FullDesc),
	    %% ++ [hr]
	    %% ++ navigation("bottom")
	    %% ++ timestamp()),
    %% XML = xhtml(Title, stylesheet(Opts), Body),
    _XML = markdown(Title, stylesheet(Opts), Body).
    %% xmerl:export_simple_content(XML, ?HTML_EXPORT).


get_first_sentence([#xmlElement{name = p, content = Es} | Tail]) ->
    %% Descend into initial paragraph.
    {First, Rest} = get_first_sentence_1(Es),
    {First,
     [#xmlElement{name = p, content = Rest} || Rest =/= []] ++ Tail};
get_first_sentence(Es) ->
    get_first_sentence_1(Es).

get_first_sentence_1(Es) ->
    get_first_sentence_1(Es, []).
    
get_first_sentence_1([E = #xmlText{value = Txt} | Es], Acc) ->
    Last = case Es of
	       [#xmlElement{name = p} | _] -> true;
	       [#xmlElement{name = br} | _] -> true;
	       [] -> true;
	       _ -> false
	   end,
    case end_of_sentence(Txt, Last) of
	{value, Txt1, Rest} ->
	    {lists:reverse([E#xmlText{value = Txt1}|Acc]),
	     if Rest == [] ->
		     Es;
		true ->
		     [#xmlText{value=Rest} | Es]
	     end};
	none ->
	    get_first_sentence_1(Es, [E | Acc])
    end;
get_first_sentence_1([E | Es], Acc) ->
    % Skip non-text segments - don't descend further
    get_first_sentence_1(Es, [E | Acc]);
get_first_sentence_1([], Acc) ->
    {lists:reverse(Acc), []}.

end_of_sentence(Cs, Last) ->
    end_of_sentence(Cs, Last, []).

%% We detect '.' and '!' as end-of-sentence markers.

end_of_sentence([$.=A, B | Cs], _, As) when B==$\s; B==$\t; B==$\n ->
    end_of_sentence_1(A, Cs, true, As);
end_of_sentence([$.=A], Last, As) ->
    end_of_sentence_1(A, [], Last, As);
end_of_sentence([$!=A, B | Cs], _, As) when B==$\s; B==$\t; B==$\n ->
    end_of_sentence_1(A, Cs, true, As);
end_of_sentence([$!=A], Last, As) ->
    end_of_sentence_1(A, [], Last, As);
end_of_sentence([C | Cs], Last, As) ->
    end_of_sentence(Cs, Last, [C | As]);
end_of_sentence([], Last, As) ->
    end_of_sentence_1($., [], Last, edoc_lib:strip_space(As)).  % add a '.'

end_of_sentence_1(C, Cs, true, As) ->
    {value, lists:reverse([C | As]), Cs};
end_of_sentence_1(_, _, false, _) ->
    none.
