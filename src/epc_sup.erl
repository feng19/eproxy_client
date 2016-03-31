%%%-------------------------------------------------------------------
%%% @author pwf
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(epc_sup).
-author("pwf").

-behaviour(supervisor).
-export([
    start_link/0,
    start_child/1
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

%% socks4
start_child(socks4) ->
    case application:get_env(eproxy_client, local_socks4_port) of
        {ok, Socks4Port} when is_integer(Socks4Port) ->
            start_socks4_child(Socks4Port);
        _ ->
            none
    end;

%% socks5
start_child(socks5) ->
    case application:get_env(eproxy_client, local_socks5_port) of
        {ok, Socks5Port} when is_integer(Socks5Port) ->
            start_socks5_child(Socks5Port);
        _ ->
            none
    end;

%% http
start_child(http) ->
    case application:get_env(eproxy_client, local_http_port) of
        {ok, HttpPort} when is_integer(HttpPort) ->
            start_http_child(HttpPort);
        _ ->
            none
    end.

start_socks4_child(Socks4Port) ->
    {ok, LSock} = gen_tcp:listen(Socks4Port, ?LISTEN_OPTIONS),
    ChildSpec = {epc_socks4_sup, {epc_socks4_sup, start_link, [LSock]},
        permanent, infinity, supervisor, [epc_socks4_sup]},
    case supervisor:start_child(?SERVER, ChildSpec) of
        {ok,_Pid} ->
            [epc_socks4_sup:start_child()||_<-lists:seq(1, worker_count())];
        R ->
            ?DEBUG("start error:~p",[R]),
            R
    end.

start_socks5_child(Socks5Port) ->
    {ok, LSock} = gen_tcp:listen(Socks5Port, ?LISTEN_OPTIONS),
    ChildSpec = {epc_socks5_sup, {epc_socks5_sup, start_link, [LSock]},
        permanent, infinity, supervisor, [epc_socks5_sup]},
    case supervisor:start_child(?SERVER, ChildSpec) of
        {ok,_Pid} ->
            [epc_socks5_sup:start_child()||_<-lists:seq(1, worker_count())];
        R ->
            ?DEBUG("start error:~p",[R]),
            R
    end.

start_http_child(HttpPort) ->
    {ok, LSock} = gen_tcp:listen(HttpPort, ?LISTEN_OPTIONS),
    ChildSpec = {epc_http_sup, {epc_http_sup, start_link, [LSock]},
        permanent, infinity, supervisor, [epc_http_sup]},
    case supervisor:start_child(?SERVER, ChildSpec) of
        {ok,_Pid} ->
            [epc_http_sup:start_child()||_<-lists:seq(1, worker_count())];
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