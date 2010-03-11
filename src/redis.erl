-module(redis).
-behaviour(gen_server).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

%% api callbacks
-export([
  start_link/1,
  q/1,
  keys/1
]).

-define(NL, <<"\r\n">>).

-record(state, {
  ip = "127.0.0.1",
  port = 6379,
  db = 0,
  pass = <<>>,
  socket = undefined
}).

%%====================================================================
%% api callbacks
%%====================================================================
start_link(Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

q(Parts) ->
  gen_server:call(?MODULE, {q, Parts}).

%%
%% Generic Sugar
%%
keys(Pat) ->
  {ok, Data} = q([keys, Pat]),
  re:split(Data, " ").

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Opts) ->
  {ok, parse_options(Opts)}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({q, Parts}, _From, State) ->
  case connect(State) of
    {ok, Socket} ->
      case send(Parts, Socket) of
        ok ->
          case read_resp(Socket) of
            {xerror, Error} ->
              {reply, Error, State#state{socket = undefined}};
            Response ->
              {reply, Response, State#state{socket = Socket}}
          end;
        Error ->
          {reply, Error, State#state{socket = undefined}}
      end;
    Error ->
      {reply, Error, State#state{socket = undefined}}
  end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

parse_options(Opts) ->
  parse_options(Opts, #state{}).
parse_options([], State) ->
  State;
parse_options([{ip, Ip} | Rest], State) ->
  parse_options(Rest, State#state{ip = Ip});
parse_options([{port, Port} | Rest], State) ->
  parse_options(Rest, State#state{port = Port});
parse_options([{db, Db} | Rest], State) ->
  parse_options(Rest, State#state{db = Db});
parse_options([{pass, Pass} | Rest], State) ->
  parse_options(Rest, State#state{pass = Pass}).

connect(State = #state{socket=undefined, ip=Ip, port=Port}) ->
  Opts = [binary, {active, false}],
  gen_tcp:connect(Ip, Port, Opts);
connect(#state{socket = Socket}) ->
  {ok, Socket}.

send(Parts, Socket) ->
  ToSend = build_request(Parts),
  gen_tcp:send(Socket, ToSend).

strip(B) when is_binary(B) ->
  S = size(B) - size(?NL),
  <<B1:S/binary, _/binary>> = B,
  B1.

read_resp(Socket) ->
  inet:setopts(Socket, [{packet, line}]),
  case gen_tcp:recv(Socket, 0) of
    {ok, Line} ->
      case Line of
        <<"*", Rest/binary>> ->
          Count = list_to_integer(binary_to_list(strip(Rest))),
          read_multi_bulk(Socket, Count, []);
        <<"+", Rest/binary>> ->
          {ok, strip(Rest)};
        <<"-", Rest/binary>> ->
          {error, strip(Rest)};
        <<":", Size/binary>> ->
          {ok, list_to_integer(binary_to_list(strip(Size)))};
        <<"$", Size/binary>> ->
          Size1 = list_to_integer(binary_to_list(strip(Size))),
          read_body(Socket, Size1);
        <<"\r\n">> ->
          read_resp(Socket);
        Uknown ->
          {unknown, Uknown}
      end;
    Error ->
      {xerror, Error}
  end.

read_body(_Socket, -1) ->
  {ok, null};
read_body(_Socket, 0) ->
  {ok, <<>>};
read_body(Socket, Size) ->
  inet:setopts(Socket, [{packet, raw}]),
  gen_tcp:recv(Socket, Size).

read_multi_bulk(_Data, 0, Acc) ->
  lists:reverse(Acc);
read_multi_bulk(Socket, Count, Acc) ->
  Acc1 = [read_resp(Socket) | Acc],
  read_multi_bulk(Socket, Count-1, Acc1).

build_request(Args) when is_list(Args) ->
  Count = length(Args),
  F = fun(V) -> ["$", to_part(length(to_part(V))), ?NL, to_part(V), ?NL] end,
  Args1 = lists:map(F, Args),
  ["*", to_part(Count), ?NL, Args1, ?NL].

to_part(A) when is_atom(A) ->
  string:to_upper(atom_to_list(A));
to_part(B) when is_binary(B) ->
  binary_to_list(B);
to_part(I) when is_integer(I) ->
  integer_to_list(I);
to_part(L) when is_list(L) ->
  L.
