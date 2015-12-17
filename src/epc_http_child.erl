-module(epc_http_child).

-behaviour(gen_server).

-export([start_link/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {key, lsock, socket, remote_pid}).

-include("socks_type.hrl").
-include("debug.hrl").
-define(TIMEOUT, 1000 * 60 * 10).
-define(HTTP_METHOD_HEADER, [
    <<"GE">>,   % GET
    <<"PO">>,   % POST
    <<"HE">>,   % HEAD
    <<"PU">>,   % PUT
    <<"DE">>,   % DELETE
    <<"TR">>,   % TRACE
    <<"CO">>,   % CONNECT
    <<"OP">>    % OPTIONS
]).


-define(INLINE_LOWERCASE(Function, Rest, A0, Acc),
    $A -> Function(Rest, A0, << Acc/binary, $a >>);
    $B -> Function(Rest, A0, << Acc/binary, $b >>);
    $C -> Function(Rest, A0, << Acc/binary, $c >>);
    $D -> Function(Rest, A0, << Acc/binary, $d >>);
    $E -> Function(Rest, A0, << Acc/binary, $e >>);
    $F -> Function(Rest, A0, << Acc/binary, $f >>);
    $G -> Function(Rest, A0, << Acc/binary, $g >>);
    $H -> Function(Rest, A0, << Acc/binary, $h >>);
    $I -> Function(Rest, A0, << Acc/binary, $i >>);
    $J -> Function(Rest, A0, << Acc/binary, $j >>);
    $K -> Function(Rest, A0, << Acc/binary, $k >>);
    $L -> Function(Rest, A0, << Acc/binary, $l >>);
    $M -> Function(Rest, A0, << Acc/binary, $m >>);
    $N -> Function(Rest, A0, << Acc/binary, $n >>);
    $O -> Function(Rest, A0, << Acc/binary, $o >>);
    $P -> Function(Rest, A0, << Acc/binary, $p >>);
    $Q -> Function(Rest, A0, << Acc/binary, $q >>);
    $R -> Function(Rest, A0, << Acc/binary, $r >>);
    $S -> Function(Rest, A0, << Acc/binary, $s >>);
    $T -> Function(Rest, A0, << Acc/binary, $t >>);
    $U -> Function(Rest, A0, << Acc/binary, $u >>);
    $V -> Function(Rest, A0, << Acc/binary, $v >>);
    $W -> Function(Rest, A0, << Acc/binary, $w >>);
    $X -> Function(Rest, A0, << Acc/binary, $x >>);
    $Y -> Function(Rest, A0, << Acc/binary, $y >>);
    $Z -> Function(Rest, A0, << Acc/binary, $z >>);
    C -> Function(Rest, A0, << Acc/binary, C >>)
).

%%%===================================================================
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%%===================================================================

init([LSock]) ->
    process_flag(trap_exit, true),
    ?DEBUG("init LSock:~p",[LSock]),
    {ok, Key} = application:get_env(eproxy_client, key),
    {ok, #state{key = Key, lsock = LSock}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(close, State) ->
    ?DEBUG("close"),
    {stop, normal, State}.

handle_info(timeout, #state{lsock = LSock, socket = undefined} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    epc_http_sup:start_child(),
    {noreply, State#state{socket = Socket}, ?TIMEOUT};

handle_info(timeout, #state{socket = Socket} = State) when is_port(Socket) ->
    ?DEBUG("timeout stop"),
    {stop, timeout, State};

%% TODO: Request is less than 2 bytes
handle_info({tcp, Socket, Request}, #state{key=Key, socket=Socket,remote_pid=undefined} = State) ->
    ?DEBUG(Request),
    case connect_to_remote(Key,Request) of
        {ok, RemotePid} ->
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{remote_pid=RemotePid}, ?TIMEOUT};
        {error, Error} ->
            ?DEBUG("error stop:~p",[Error]),
            {stop, Error, State}
    end;

handle_info({tcp, Socket, <<Header:2/binary, _Rest/binary>> = Request}, #state{key=Key, socket=Socket,remote_pid=RemotePid} = State) ->
    ?DEBUG(Request),
    case lists:member(Header, ?HTTP_METHOD_HEADER) of
        true ->
            % New Request In Same connection. HTTP/1.1
            case connect_to_remote(Key,Request) of
                {ok, RemotePid} ->
                    ok = inet:setopts(Socket, [{active, once}]),
                    {noreply, State#state{remote_pid=RemotePid}, ?TIMEOUT};
                {error, Error} ->
                    ?DEBUG("error stop:~p",[Error]),
                    {stop, Error, State}
            end;
        false ->
            epc_ws_handler:send(RemotePid, epc_crypto:encrypt(Key, Request)),
            {noreply, State, ?TIMEOUT}
    end;

%% recv from remote, and send back to client
handle_info({websocket_msg,Response}, #state{key=Key, socket=Client} = State) ->
    {ok, RealData} = epc_crypto:decrypt(Key, Response),
    case gen_tcp:send(Client, RealData) of
        ok ->
            ok = inet:setopts(Client, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            ?DEBUG("error stop:~p",[Error]),
            {stop, Error, State}
    end;


handle_info({tcp_closed, _}, State) ->
    ?DEBUG("tcp_closed stop"),
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    ?DEBUG("tcp_error stop:~p",[Reason]),
    {stop, Reason, State};

handle_info({'EXIT', Pid, Reason}, #state{remote_pid=RemotePid}=State) ->
    case Reason of
        shutdown ->
            {stop, Reason, State};
        _ when Pid==RemotePid ->
            {stop, Reason, State};
        _ ->
            {noreply, State}
    end.


terminate(Reason, #state{socket=Socket, remote_pid=RemotePid}) ->
    ?DEBUG("terminate:~p",[Reason]),
    case is_pid(RemotePid) of
        true ->
            epc_ws_handler:close(RemotePid);
        false ->
            ok
    end,
    case is_port(Socket) of
        true -> 
            gen_tcp:close(Socket);
        false -> 
            ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

connect_to_remote(Key,Request) ->
    {ok, RemoteAddr} = application:get_env(eproxy_client, remote_addr),
    case epc_ws_handler:start_link(RemoteAddr,self()) of
        {ok, RemotePid} ->
            case parse_request(Request) of
                {ok, Target, NormalizedReqeust} ->
                    epc_ws_handler:send(RemotePid, epc_crypto:encrypt(Key, Target)),
                    epc_ws_handler:send(RemotePid, epc_crypto:encrypt(Key, NormalizedReqeust)),
                    {ok,RemotePid};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Error} ->
            {error, Error}
    end.

parse_request(Request) ->
    case binary:split(Request, <<"\r\n">>) of
        [Request] ->
            % TODO Need recv more...
            {error, need_more};
        [FirstLine, RestLines] ->
            [Method, Uri, Version] = binary:split(FirstLine, <<" ">>, [global]),
            case parse_fullhost(Uri) of
                {_, undefined} ->
                    {error, {parse_error,Uri}};
                {Domain, Port} ->
                    case catch ( normalize_reqeust(Method, Uri, Version, RestLines) ) of
                        {ok, NormalizedRequest} ->
                            {ok, <<?DOMAIN, Port:16, (byte_size(Domain)):8, Domain/binary>>, NormalizedRequest};
                        Error ->
                            {error,Error}
                    end
            end
    end.


%% @doc Extract host and port from a binary.
%%
%% Because the hostname is case insensitive it is converted
%% to lowercase.

-spec parse_fullhost(binary()) -> {binary(), undefined | non_neg_integer()}.
parse_fullhost(Fullhost) ->
    parse_fullhost(Fullhost, false, <<>>).

parse_fullhost(<< $[, Rest/bits >>, false, <<>>) ->
    parse_fullhost(Rest, true, << $[ >>);
parse_fullhost(<<>>, false, Acc) ->
    {Acc, undefined};
%% @todo Optimize.
parse_fullhost(<< $:, Rest/bits >>, false, Acc) ->
    {Acc, list_to_integer(binary_to_list(Rest))};
parse_fullhost(<< $], Rest/bits >>, true, Acc) ->
    parse_fullhost(Rest, false, << Acc/binary, $] >>);
parse_fullhost(<< C, Rest/bits >>, E, Acc) ->
    case C of
        ?INLINE_LOWERCASE(parse_fullhost, Rest, E, Acc)
    end.

normalize_reqeust(Method, Uri, Version, RestLines) ->
    UriSplited = binary:split(Uri, <<"/">>, [global]),

    case UriSplited of
        [_, _, _] ->
            Normalized = <<Method/binary, <<" ">>/binary, <<"/">>/binary, <<" ">>/binary,  Version/binary, <<"\r\n">>/binary, RestLines/binary>>,
            {ok, Normalized};
        [_, _, _ | Paths] ->
            Path =  lists:foldr(
                fun(Item, Acc) -> <<Item/binary, Acc/binary>> end,
                <<>>,
                lists:map(fun(Item) -> << <<"/">>/binary, Item/binary >> end, Paths)
            ),
            Normalized = <<Method/binary, <<" ">>/binary, Path/binary, <<" ">>/binary,  Version/binary, <<"\r\n">>/binary,  RestLines/binary >>,
            {ok, Normalized};
        _ ->
            Normalized = <<Method/binary, <<" ">>/binary, <<"/">>/binary, <<" ">>/binary,  Version/binary, <<"\r\n">>/binary, RestLines/binary>>,
            {ok, Normalized}
            %{error, {split_failure, UriSplited}}
    end.
