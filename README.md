# redis-erl

## Synopsis

  minimalistic [redis](http://code.google.com/p/redis/) client for erlang

## Overview

  redis-erl is a redis client implementation that only
  uses redis's [multi bulk command protocol](http://code.google.com/p/redis/wiki/ProtocolSpecification).

## Install

*with [epm](http://github.com/JacobVorreuter/epm)*

  ensure you have the latest version of epm:

    $ epm latest

  install

    $ epm install bmizerany/redis-erl

*with [rebar](http://bitbucket.org/basho/rebar)*

    $ rebar compile

## Raw examples

    0> redis:connect([]).  % connect to default redis port on localhost
    1> redis:q([set, "foo", "bar"]).
    {ok,<<"OK">>}
    2> redis:q([get, "foo"]).
    {ok,<<"bar">>}
    3> redis:q([sadd, "bar", "a"]).
    {ok,1}
    4> redis:q([sadd, "bar", "b"]).
    {ok,1}
    5> redis:q([sadd, "bar", "c"]).
    {ok,1}
    6> redis:q([lrange, 0, -1]).
    {error,<<"ERR wrong number of arguments for 'lrange' command">>}
    7> redis:q([lrange, "bar", 0, -1]).
    {error,<<"ERR Operation against a key holding the wrong kind of value">>}
    8> redis:q([smembers, "bar"]).
    [{ok,<<"c">>},{ok,<<"a">>},{ok,<<"b">>}]
    9> redis:q([incr, "counter"]).
    {ok,1}
    10>

## Breakdance/Breakdown

    redis:connect(Options) -> ok | {error, Reason}

      Types:

        Options = [Opt]
        Opt = {atom(), Value}
        Value = term()

      Connects to redis server

      The available options are:

      {ip, Ip}

        If the host has several network interfaces, this option
        specifies which one to use.  Default is "127.0.0.1".

      {port, Port}

        Specify which local port number to use.  Default is 6379.

      {pass, Pass}

        Specify to password to auth with upon a successful connect.
        Default is <<>>.

      {db, DbIndex}

        Sepcify the db to use.  Default is 0.


    redis:q(Parts) -> {ok, binary()} | {ok, int()} | {error, binary()}

      Types

        Parts = [Part]

          The sections, in order, of the command to be executed

        Part = atom() | binary() | list()

          A section of a redis command

          NOTE:  An atom() is converted to an uppercase list().

      Tell redis to execute a command.

    redis:keys(Pat) -> [binary()]

      Types

        Pat = list()

          The pattern to search for keys on

      See: http://code.google.com/p/redis/wiki/KeysCommand

## In motion

The amazing [SORT](http://code.google.com/p/redis/wiki/SortCommand) command:

    redis:q([sort, "foo", by, "bar:*"]).

    is converted to:

    ["SORT", "foo", "BY", "bar:*"]

    which is then compiled to

    *4
    $4
    SORT
    $3
    foo
    $2
    BY
    $5
    bar:*

    and sent over the wire to redis.

## Sugar

Sugar functions enable you to avoid the slightly less attractive
raw `redis:q/1` command.

There is currently only one sugar command, `redis:keys/1`

    1> redis:keys("f*").
    [<<"foo">>, <<"funk">>]

## On Sugar commands and future additions too

Sugar commands must return an easy to use `term()` that resembles
the closest erlang-esq expression of it's redis form.

I'm currently on the fence about how these commands look,
and what they return, but thanks to the multi-bulk-request
protocol redis uses, it's simple to use `redis:q/1` function
for *all* commands.

How can you get your Sugar functions in?  See the next section.

## Contribute

I'm very open to patches!  Help is greatly appreciated.

- Fork this repo
- Add an issue to this repos issues with a link to you commit/compare-view
- I'll get to it as soon as possible

## LICENSE
Copyright (c) 2010 Blake Mizerany

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
