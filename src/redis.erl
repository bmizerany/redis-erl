-module(redis).
-behaviour(gen_server).

%% gen_server exports
-export([
  init/1,
  handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3
]).

-export([
  connect/1,
  q/1,
  keys/1
]).

-define(NL, "\r\n").
-define(DEFAULTS, [
  {ip, "127.0.0.1"},
  {port, 6379},
  {db, 0},
  {pass, <<>>}
]).

%% --------------------
%% Publi API
%% --------------------

connect(PropList) ->
  PropList1 = set_defaults(?DEFAULTS, PropList),
  Result = gen_server:start_link({local, ?MODULE}, ?MODULE, PropList1, []),
  case Result of
    {ok, _Pid} ->
      case q([auth, proplists:get_value(pass, PropList1)]) of
        {ok, _} ->
          q([select, proplists:get_value(db, PropList1)]);
        Error ->
          Error
      end;
    Error ->
      Error
  end.

q(Parts) ->
  gen_server:call(?MODULE, {q, Parts}).


%%
%% Generic Sugar
%%

keys(Pat) ->
  {ok, Data} = q([keys, Pat]),
  re:split(Data, " ").

%% --------------------
%% Private API
%% --------------------

set_default({Prop, Value}, PropList) ->
  case proplists:is_defined(Prop, PropList) of
    true ->
      PropList;
    false ->
      [{Prop, Value} | PropList]
  end.

set_defaults(Defaults, PropList) ->
  lists:foldl(fun set_default/2, PropList, Defaults).

strip(B) when is_binary(B) ->
  S = size(B) - 2,
  <<B1:S/binary, _/binary>> = B,
  B1.

read_resp(Socket) ->
  inet:setopts(Socket, [{packet, line}]),
  {ok, Line} = gen_tcp:recv(Socket, 0),
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


%% ----------
%% gen_server
%% ----------

init(Opts) ->
  Ip = proplists:get_value(ip, Opts),
  Port = proplists:get_value(port, Opts),
  SocketOpts = [binary, {packet, line}, {active, false}, {recbuf, 1024}],
  io:format("opts: ~p ~p ~p ~p~n", [Opts, Ip, Port, SocketOpts]),
  Result = gen_tcp:connect(Ip, Port, SocketOpts),
  case Result of
    {ok, Socket} ->
      {ok, Socket};
    Error ->
      {stop, Error}
  end.

handle_call({q, Parts}, _From, Socket) ->
  ToSend = build_request(Parts),
  io:format("SENDING:~n~p~n", [ToSend]),
  Result = case gen_tcp:send(Socket, ToSend) of
    ok ->
      read_resp(Socket);
    Error ->
      Error
  end,
  {reply, Result, Socket}.

handle_cast(_, _) -> ok.
handle_info(_, _) -> ok.
terminate(_, _) -> ok.
code_change(_, _, _) -> ok.
