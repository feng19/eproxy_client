%% @author pwf
%% @doc 

-module(epc_remote_addr_list).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/0,
    get_remote_addr_list/0,
    get_remote_addr/0,
    reload_remote_addr_list/0
]).

-record(state, {len=0,now=1}).

%% ====================================================================

start_link() ->
    RemoteAddrList = get_remote_addr_list(),
    gen_server:start_link({local,?MODULE} ,?MODULE, [RemoteAddrList], []).

get_remote_addr_list() ->
    case application:get_env(eproxy_client, remote_addr_list) of
        {ok, RemoteAddrList0} ->
            RemoteAddrList0;
        _ ->
            {ok, Key} = application:get_env(eproxy_client, key),
            {ok, RemoteAddr} = application:get_env(eproxy_client, remote_addr),
            [{RemoteAddr,Key}]
    end.

get_remote_addr() ->
    gen_server:call(?MODULE, get_remote_addr).

reload_remote_addr_list() ->
    RemoteAddrList = get_remote_addr_list(),
    gen_server:cast(?MODULE, {reload_remote_addr_list,RemoteAddrList}).

%% ====================================================================

init([RemoteAddrList]) ->
    Len = set_remote_addr_list(RemoteAddrList),
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

handle_cast({reload_remote_addr_list,RemoteAddrList}, State) ->
    Len = set_remote_addr_list(RemoteAddrList),
    {noreply, State#state{now=1,len=Len}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================

set_remote_addr_list(RemoteAddrList) ->
    lists:foldl(
        fun({RemoteAddr0,Key0},N) ->
            case {check_url(RemoteAddr0), check_key(Key0)} of
                {{ok, RemoteAddr}, {ok, Key}} ->
                    NewN = N+1,
                    put(NewN,{RemoteAddr, Key}),
                    NewN;
                {{error,Error}, _} ->
                    io:format("url:~p key:~p,url error:~p~n",[RemoteAddr0, Key0, Error]),
                    N;
                {_, {error,Error}} ->
                    io:format("url:~p key:~p,key error:~p~n",[RemoteAddr0, Key0, Error]),
                    N
            end
        end, 0, RemoteAddrList).

check_key(Key0) ->
    case to_binary(Key0) of
        Key when size(Key)==16 ->
            {ok, Key};
        _ ->
            {error, invalid_key}
    end.

check_url(URL0) ->
    URL = to_list(URL0),
    case http_uri:parse(URL, [{scheme_defaults, [{ws,80},{wss,443}]}]) of
        {ok, _} ->
            {ok, to_binary(URL)};
        Error ->
            Error
    end.

to_list(L) when is_list(L) -> L;
to_list(B) when is_binary(B) -> binary_to_list(B).

to_binary(A) when is_atom(A) -> to_binary(atom_to_list(A));
to_binary(B) when is_binary(B) -> B;
to_binary(I) when is_integer(I) -> to_binary(integer_to_list(I));
to_binary(L) when is_list(L) -> iolist_to_binary(L).