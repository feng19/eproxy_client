-module(epc_ws_handler).

-behaviour(websocket_client_handler).

-export([
    start_link/2,
    send/2,
    close/1
]).
-export([
    init/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

-include("socks_type.hrl").
-include("debug.hrl").

%% ===================================================================
start_link(RemoteAddr,LinkPid) ->
    websocket_client:start_link(RemoteAddr, ?MODULE, [LinkPid]).

send(HanderPid,Binary) ->
    ?DEBUG("hander_pid:~p,send:~p",[HanderPid,Binary]),
    websocket_client:cast(HanderPid, {binary, Binary}).

close(HanderPid) ->
    ?DEBUG("hander_pid:~p,close",[HanderPid]),
    websocket_client:cast(HanderPid, close).

%% ===================================================================

init([LinkPid], _ConnState) ->
    ?DEBUG("socket start:~p",[LinkPid]),
    {ok, LinkPid}.

websocket_handle({pong, _}, _ConnState, State) ->
    ?DEBUG("pong"),
    {ok, State};
websocket_handle({binary, Binary}, _ConnState, LinkPid) ->
    ?DEBUG("Received msg ~p~n", [Binary]),
    LinkPid ! {websocket_msg,Binary},
    {ok, LinkPid}.

websocket_info(close, _ConnState, State) ->
    ?DEBUG("epc_websocket_handler close"),
    {close, <<>>, State};
websocket_info(Msg, _ConnState, State) ->
    ?DEBUG(Msg),
    {ok, State}.

websocket_terminate(Reason, _ConnState, State) ->
    ?DEBUG("Websocket closed in state ~p wih reason ~p",[State, Reason]),
    ok.

%% ===================================================================


