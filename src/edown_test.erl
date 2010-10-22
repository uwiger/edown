-module(edown_test).

-export([foo/1]).

-include_lib("edoc/include/edoc_doclet.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% @doc test module.
%% @end

foo(#context{}) ->
    x.
