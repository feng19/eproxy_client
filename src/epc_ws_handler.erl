-module(epc_ws_handler).
-include("socks_type.hrl").
-include("debug.hrl").
-behaviour(websocket_client_handler).

-export([ init/2, websocket_handle/3, websocket_info/3, websocket_terminate/3 ]).
-export([
    start_link/1,
    send/2
]).

-record(state, {key, link_pid}).

%% ===================================================================
start_link(LinkPid) ->
%%     {ok, Key} = application:get_env(eproxy_client, key),
%%     {ok, RemoteAddr} = application:get_env(eproxy_client, remote_addr),
    {RemoteAddr,Key} = epc_remote_addr_list:get_remote_addr(),
    websocket_client:start_link(RemoteAddr, ?MODULE, [Key,LinkPid]).

send(HanderPid,Request) ->
%%     ?DEBUG("hander_pid:~p,send:~p",[HanderPid,Request]),
    HanderPid ! Request.

%% ===================================================================

init([Key,LinkPid], _ConnState) ->
    ?DEBUG("ws handler start:~p",[LinkPid]),
    process_flag(trap_exit, true),
    {ok, #state{key=Key, link_pid=LinkPid}}.

websocket_handle({binary, Binary}, _ConnState, #state{key=Key,link_pid=LinkPid}=State) ->
%%    ?DEBUG("Received msg ~p~n", [Binary]),
    {ok, Response} = epc_crypto:decrypt(Key, Binary),
    LinkPid ! {websocket_msg, Response},
    {ok, State};
websocket_handle({pong, _}, _ConnState, State) ->
%%     ?DEBUG("pong"),
    {ok, State}.

websocket_info(Request, _ConnState, #state{key=Key}=State) when is_binary(Request) ->
%%    ?DEBUG("Request:~p", [Request]),
    Binary = epc_crypto:encrypt(Key, Request),
    {reply, {binary, Binary}, State};
websocket_info({'EXIT',LinkPid,normal}, _ConnState, #state{link_pid=LinkPid}=State) ->
    {close, <<>>, State};
websocket_info({'EXIT',LinkPid,Reason}, _ConnState, #state{link_pid=LinkPid}=State) ->
    ?DEBUG("exit:~p",[Reason]),
    {close, <<>>, State};
websocket_info(_, _ConnState, State) ->
    {reply, none, State}.

websocket_terminate(_Reason, _ConnState, _State) ->
%%     ?DEBUG("Websocket closed in state ~p wih reason ~p",[State, Reason]),
    ok.

%% ===================================================================


