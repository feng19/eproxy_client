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
    start_child/2
]).
-export([init/1]).

-include("debug.hrl").

-define(SERVER, ?MODULE).


%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% socks4
start_child(socks4,LocalPort) ->
    {ok, LSock} = gen_tcp:listen(LocalPort, [binary,
        {ip, {0, 0, 0, 0}},
        {reuseaddr, true},
        {active, false},
        {backlog, 256}]),
    ChildSpec = {epc_socks4_sup, {epc_socks4_sup, start_link, [LSock]},
                 permanent, infinity, supervisor, [epc_socks4_sup]},
    case supervisor:start_child(?SERVER, ChildSpec) of
        {ok,_Pid} ->
            epc_socks4_sup:start_child();
        R ->
            ?DEBUG("start error:~p",[R]),
            R
    end;

%% socks5
start_child(socks5,LocalPort) ->
    {ok, LSock} = gen_tcp:listen(LocalPort, [binary,
        {ip, {0, 0, 0, 0}},
        {reuseaddr, true},
        {active, false},
        {backlog, 256}]),
    ChildSpec = {epc_socks5_sup, {epc_socks5_sup, start_link, [LSock]},
                 permanent, infinity, supervisor, [epc_socks5_sup]},
    case supervisor:start_child(?SERVER, ChildSpec) of
        {ok,_Pid} ->
            epc_socks5_sup:start_child();
        R ->
            ?DEBUG("start error:~p",[R]),
            R
    end;

%% http
start_child(http,LocalPort) ->
    {ok, LSock} = gen_tcp:listen(LocalPort, [binary,
        {ip, {0, 0, 0, 0}},
        {reuseaddr, true},
        {active, once},
        {backlog, 256}]),
    ChildSpec = {epc_http_sup, {epc_http_sup, start_link, [LSock]},
                 permanent, infinity, supervisor, [epc_http_sup]},
    case supervisor:start_child(?SERVER, ChildSpec) of
        {ok,_Pid} ->
            epc_http_sup:start_child();
        R ->
            ?DEBUG("start error:~p",[R]),
            R
    end;

start_child(Scheme,LocalPort) ->
    ?DEBUG({unknow_scheme,Scheme,LocalPort}),
    {false,unknow_scheme}.

%%%===================================================================
init([]) ->
    SupFlags = {one_for_one, 10, 1},
    {ok, {SupFlags, []}}.

