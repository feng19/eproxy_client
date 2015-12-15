-module(epc_http).

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

-record(state, {key, lsock, socket, remote}).

-include("socks_type.hrl").
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

%%%===================================================================
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%%===================================================================

init([LSock]) ->
    {ok, Key} = application:get_env(eproxy_client, key),
    {ok, #state{key = Key, lsock = LSock}, 0}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({http_new, Socket, Request}, #state{key = Key, socket = Socket} = State) ->
    {ok, Remote} = connect_to_remote(),
    case parse_request(Request) of
        {ok, Target, NormalizedReqeust} ->
            ok = gen_tcp:send(Remote, epc_crypto:encrypt(Key, Target)),
            ok = gen_tcp:send(Remote, epc_crypto:encrypt(Key, NormalizedReqeust)),
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{remote = Remote}, ?TIMEOUT};
        {error, Reason} ->
            {stop, Reason, State}
    end;


handle_cast({http_continue, Socket, Request}, #state{key = Key, socket = Socket, remote = Remote} = State) ->
    NewRemote =
        case is_port(Remote) of
            true ->
                Remote;
            false ->
                {ok, R} = connect_to_remote(),
                R
        end,

    ok = gen_tcp:send(NewRemote, epc_crypto:encrypt(Key, Request)),
    {noreply, State#state{remote = NewRemote}, ?TIMEOUT};


handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(timeout, #state{lsock = LSock, socket = undefined} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    epc_http_sup:start_child(),
    {noreply, State#state{socket = Socket}, ?TIMEOUT};

handle_info(timeout, #state{socket = Socket} = State) when is_port(Socket) ->
    {stop, timeout, State};


%% TODO: Request is less than 2 bytes
handle_info({tcp, Socket, <<Header:2/binary, _Rest/binary>> = Request}, #state{socket=Socket} = State) ->
    case lists:member(Header, ?HTTP_METHOD_HEADER) of
        true ->
            % New Request In Same connection. HTTP/1.1
            gen_server:cast(self(), {http_new, Socket, Request});
        false ->
            gen_server:cast(self(), {http_continue, Socket, Request})
    end,
    {noreply, State, ?TIMEOUT};


%% recv from remote, and send back to client
handle_info({tcp, Socket, Response}, #state{key=Key, socket=Client} = State) when Socket /= Client ->
    {ok, RealData} = epc_crypto:decrypt(Key, Response),
    case gen_tcp:send(Client, RealData) of
        ok ->
            ok = inet:setopts(Socket, [{active, once}]),
            {noreply, State, ?TIMEOUT};
        {error, Error} ->
            {stop, Error, State}
    end;


handle_info({tcp_closed, Socket}, #state{socket = Client, remote = Remote} = State) ->
    case Socket /= Client andalso Socket /= Remote of
        true ->
            % old connection
            {noreply, State, ?TIMEOUT};
        false ->
            {stop, normal, State}
    end;


handle_info({tcp_error, Socket, Reason}, #state{socket = Client, remote = Remote} = State) ->
    case Socket /= Client andalso Socket /= Remote of
        true ->
            % old connection
            {noreply, State, ?TIMEOUT};
        false ->
            {stop, Reason, State}
    end.


terminate(_Reason, #state{socket=Socket, remote=Remote}) ->
    case is_port(Socket) of
        true -> gen_tcp:close(Socket);
        false -> ok
    end,

    case is_port(Remote) of
        true -> gen_tcp:close(Remote);
        false -> ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================

connect_to_remote() ->
    {ok, RemoteAddr} = application:get_env(eproxy_client, remote_addr),
    {ok, RemotePort} = application:get_env(eproxy_client, remote_port),
    {ok, Addr} = inet:getaddr(RemoteAddr, inet),

    gen_tcp:connect(Addr, RemotePort, [binary, {active, once}, {packet, 4}]).


-spec parse_request(Request :: binary()) -> {ok, {Target :: binary(), NormalizedRequest :: binary()}} |
                                             {error, Reason :: term()}.
parse_request(Request) ->
    case binary:split(Request, <<"\r\n">>) of
        [Request] ->
            % TODO Need recv more...
            {error, need_more};
        [FirstLine, RestLines] ->
            [Method, Uri, Version] = binary:split(FirstLine, <<" ">>, [global]),
            try
                {ok, {Host, Port}} = find_target(Uri),
                {ok, NormalizedRequest} = normalize_reqeust(Method, Uri, Version, RestLines),

                DomainLen = length(Host),
                Domain = list_to_binary(Host),
                {ok, <<?DOMAIN, Port:16, DomainLen:8, Domain/binary>>, NormalizedRequest}
            catch
                error: {badmatch, Reason}  ->
                    {error, {badmatch, Reason}}
            end
    end.


find_target(Uri) ->
    case http_uri:parse(binary_to_list(Uri)) of
        {ok, {_Sheme, _UserInfo, Host, Port, _Path, _Query}} ->
            {ok, {Host, Port}};
        {ok, {_Sheme, _UserInfo, Host, Port, _Path, _Query, _Fragment}} ->
            {ok, {Host, Port}};
        {error, Reason} ->
            {error, Reason}
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
            {error, {split_failure, UriSplited}}
    end.
