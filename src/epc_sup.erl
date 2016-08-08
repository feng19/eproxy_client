-module(epc_sup).

-behaviour(supervisor).
-export([
    start_link/0,
    start_child/2
]).
-export([init/1]).

-include("debug.hrl").

-define(SERVER, ?MODULE).
-define(LISTEN_OPTIONS,
    [binary, {ip, {0, 0, 0, 0}}, {reuseaddr, true},
        {active, false}, {backlog, 256}]).


%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Key, Sup) ->
    case application:get_env(Key) of
        {ok, Port} when is_integer(Port) ->
            start_child_do(Port, Sup);
        _ ->
            none
    end.

start_child_do(Port, Sup) ->
    {ok, LSock} = gen_tcp:listen(Port, ?LISTEN_OPTIONS),
    ChildSpec = {Sup, {Sup, start_link, [LSock]},
        permanent, infinity, supervisor, [Sup]},
    case supervisor:start_child(?SERVER, ChildSpec) of
        {ok,_Pid} ->
            [Sup:start_child()||_<-lists:seq(1, worker_count())];
        R ->
            ?DEBUG("start error:~p",[R]),
            R
    end.

%%%===================================================================
init([]) ->
    SupFlags = {one_for_one, 10, 1},
    ChildSpecs = [
        {epc_remote_addr_list, {epc_remote_addr_list, start_link, []},
         permanent, infinity, worker, [epc_remote_addr_list]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

worker_count() ->
    min(8, erlang:system_info(schedulers_online)*2).