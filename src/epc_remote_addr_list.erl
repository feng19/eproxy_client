%% @author pwf
%% @doc 

-module(epc_remote_addr_list).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/0,
    get_remote_addr/0
]).

-record(state, {len=0,now=1}).

%% ====================================================================

start_link() ->
    RemoteAddrList =
    case application:get_env(eproxy_client, remote_addr_list) of
        {ok, RemoteAddrList0} ->
            RemoteAddrList0;
        _ ->
            {ok, Key} = application:get_env(eproxy_client, key),
            {ok, RemoteAddr} = application:get_env(eproxy_client, remote_addr),
            [{RemoteAddr,Key}]
    end,
    gen_server:start_link({local,?MODULE} ,?MODULE, [RemoteAddrList], []).

get_remote_addr() ->
    gen_server:call(?MODULE, get_remote_addr).

%% ====================================================================

init([RemoteAddrList]) ->
    Len = 
    lists:foldl(
        fun({_RemoteAddr,_Key}=T,N) ->
            NewN = N+1,
            put(NewN,T),
            NewN
        end, 0, RemoteAddrList),
    {ok, #state{len=Len}}.

handle_call(get_remote_addr, _From, State) ->
    N = State#state.now,
    NewN =
    if
        N==State#state.len ->
            1;
        true ->
            N+1
    end,
    {reply, get(N), State#state{now=NewN}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================


