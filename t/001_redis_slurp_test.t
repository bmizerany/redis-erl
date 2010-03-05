#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin/ -boot start_sasl

main(_) ->

  %% Devil Redis
  redis:connect("127.0.0.1", 6660),
  etap:plan(unknown),

  %% Test flushdb first to ensure we have a clean db
  etap:is(
    redis:q([flushdb]),
    {ok, <<"OK">>},
    "flushdb"
  ),

  etap:is(
    redis:q([set, "foo", "bar"]),
    {ok, <<"OK">>},
    "test status reply"
  ),

  etap:is(
    redis:q([get, "foo"]),
    {ok, <<"bar">>},
    "test bulk reply"
  ),

  redis:q([set, "abc", "123"]),
  etap:is(
    redis:q([mget, "foo", "abc"]),
    [{ok, <<"bar">>}, {ok, <<"123">>}],
    "multi bulk reply"
  ),

  ok.
