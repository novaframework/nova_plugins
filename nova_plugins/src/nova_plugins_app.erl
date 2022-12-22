%%%-------------------------------------------------------------------
%% @doc nova_plugins public API
%% @end
%%%-------------------------------------------------------------------

-module(nova_plugins_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    nova_plugins_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
