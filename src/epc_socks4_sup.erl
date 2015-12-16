-module(epc_socks4_sup).

-behaviour(supervisor).

-export([
    start_link/1,
    start_child/0
]).

-export([init/1]).
-define(SERVER, ?MODULE).

%%===================================================================
start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
    supervisor:start_child(?SERVER, []).

%%===================================================================
init([LSock]) ->
    Child = {epc_socks4_child, {epc_socks4_child, start_link, [LSock]},
             temporary, brutal_kill, worker, [epc_socks4_child]},

    Children = [Child],
    Restart = {simple_one_for_one, 0, 1},
    {ok, {Restart, Children}}.

