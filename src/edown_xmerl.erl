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

%% Description  : Callback module for exporting XML to Github-flavored Markdown.

-module(edown_xmerl).

-export(['#xml-inheritance#'/0]).

%% Note: we assume XML data, so all tags are lowercase!

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1]).

-import(xmerl_lib, [markup/3, find_attribute/2, export_text/1]).

-include_lib("xmerl/include/xmerl.hrl").


'#xml-inheritance#'() -> [].


%% The '#text#' function is called for every text segment.

'#text#'(Text) ->
    %% export_text(Text).
    try normalize(binary_to_list(list_to_binary(Text)))
    catch
	error:_ ->
	    lists:flatten(io_lib:fwrite("~p", [Text]))
    end.

normalize("\n" ++ [H|T]) when H==$\s;
			      H==$\t ->
    normalize("\n" ++ T);
normalize([H|T]) ->
    [H|normalize(T)];
normalize([]) ->
    [].




%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.

'#root#'(Data, Attrs, [], _E) -> 
    case find_attribute(header, Attrs) of
	{value, Hdr} ->
	    [lists:flatten(io_lib:fwrite("HEADER: ~p~n", [Hdr])), Data];
	false ->
	    Data
    end.

%% Note that SGML does not have the <Tag/> empty-element form.
%% Furthermore, for some element types, the end tag may be forbidden -
%% this can be handled by extending this module - see xmerl_otpsgml.erl
%% for an example. (By default, we always generate the end tag, to make
%% sure that the scope of a markup is not extended by mistake.)

'#element#'('div', Data, _, _Parents, _E) ->
    %% special case - we use 'div' to enforce html encoding
    Data;
'#element#'(Tag, Data, Attrs, Parents, E) ->
    case needs_html(Tag) orelse within_html(Parents) of
	true ->
	    html_elem(Tag, Data, Attrs, Parents, E);
	false ->
	    elem(Tag, Data, Attrs, Parents, E)
    end.

html_elem(Tag, Data, Attrs, Parents, E) ->
    HTML = fun() ->
		   xmerl_html:'#element#'(Tag, Data, Attrs, Parents, E)
	   end,
    case within_html(Parents) of
	true ->
	    HTML();
	false ->
	    ["\n\n", HTML(), "\n\n"]
    end.

elem(a, Data, Attrs, _Parents, _E) ->
    %% io:fwrite("A TAG = ~p~nPs = ~p~n", [Data, _Parents]),
    case lists:keyfind(href, #xmlAttribute.name, Attrs) of
	#xmlAttribute{value = HRef}  ->
	    "[" ++ Data ++ "](" ++ HRef ++ ")";
	false ->
	    case lists:keyfind(name, #xmlAttribute.name, Attrs) of
		#xmlAttribute{} ->
		    ["\n",
		     xmerl_lib:start_tag(a,Attrs),
		     Data,
		     xmerl_lib:end_tag(a),
		    "\n"]
	    end
    end;
elem(img, _Data, Attrs, _Parents, _E) ->
    #xmlAttribute{value = Src} = lists:keyfind(src,#xmlAttribute.name,Attrs),
    #xmlAttribute{value = Alt} = lists:keyfind(alt,#xmlAttribute.name,Attrs),
    "![" ++ Alt ++ "](" ++ Src ++ ")";
elem(li, Data, _Attrs, [{ul,_}|_], _E) ->
    "* " ++ Data ++ "\n";
elem(li, Data, _Attrs, [{ol,_}|_], _E) ->
    "1. " ++ Data ++ "\n";
elem(Tag, Data, Attrs, Parents, E) ->
    case Tag of
	title ->
	    %% io:fwrite("TITLE = |~s|~n", [Data]),
	    Str = lists:flatten(Data),
	    Str ++ "\n" ++ [$= || _ <- Str] ++ "\n";
	html  -> Data;
	body  -> Data;
	'div' -> Data;
	ul    -> Data;
	ol    -> Data;
	p     -> "\n\n" ++ Data;
	b     -> "__" ++ no_nl(Data) ++ "__";
	em    -> "_" ++ no_nl(Data) ++ "_";
	i     -> "_" ++ no_nl(Data) ++ "_";
	tt    -> "`" ++ no_nl(Data) ++ "`";
	code  -> "`" ++ no_nl(Data) ++ "`";
	dl    -> Data;
	dt    -> html_elem(h3, Data, Attrs, Parents, E);
	dd    -> html_elem(p, Data, Attrs, Parents, E);
	h1 -> "\n\n#" ++ no_nl(Data) ++ "#\n";
	h2 -> "\n\n##" ++ no_nl(Data) ++ "##\n";
	h3 -> "\n\n###" ++ no_nl(Data) ++ "##\n";
	h4 -> "\n\n####" ++ no_nl(Data) ++ "##\n";
	hr -> "---------\n";
	head -> [];
	_ ->
		    ["\n",
		     xmerl_lib:start_tag(Tag,Attrs),
		     Data,
		     xmerl_lib:end_tag(Tag),
		    "\n"]
    end.

within_html(Tags) ->
    lists:any(fun({T,_}) -> needs_html(T) end, Tags).

needs_html(T) ->
    lists:member(T, [table,'div',h1,h2,h3,h4,dd,dt]).

no_nl(S) ->
    string:strip([C || C <- lists:flatten(S),
		       C =/= $\n], both).

%% attr(#xmlAttribute{name = N, value = V}) ->
%%     "(" ++ atom_to_list(N) ++ "=" ++ [a_val(V)] ++ ")".

%% a_val(V) when is_atom(V) ->
%%     atom_to_list(V);
%% a_val(V) when is_integer(V) ->
%%     integer_to_list(V);
%% a_val(V) ->
%%     V.




