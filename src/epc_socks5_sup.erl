-module(epc_socks5_sup).

-behaviour(supervisor).

-export([
    start_link/2,
    start_child/0
]).

-export([init/1]).
-define(SERVER, ?MODULE).

%%===================================================================
start_link(LSock,LocalPort) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock,LocalPort]).

start_child() ->
    supervisor:start_child(?SERVER, []).

%%===================================================================
init([LSock,LocalPort]) ->
    Child = {epc_socks5_child, {epc_socks5_child, start_link, [LSock,LocalPort]},
             temporary, brutal_kill, worker, [epc_socks5_child]},

    Children = [Child],
    Restart = {simple_one_for_one, 0, 1},
    {ok, {Restart, Children}}.

