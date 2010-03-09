# redis-erl

## Synopsis

   minimalistic redis client for erlang

## Install

  ensure you have the latest version of epm:

    $ epm latest

  install

    $ epm install bmizerany/redis-erl

or add this repo to your `include` directory.

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

## Sugar

Sugar functions enable you to avoid the slightly less attractive
raw redis:q/1 command.

There is currently only one sugar command, `redis:keys/1`

    1> redis:keys("f*").
    [<<"foo">>, <<"funk">>]

## On Sugar commands and additions

Sugar commands must return an easy to use term() that resembles
the closest erlang-esque expression of it's form.

## Contribute

I'm very open to patches!  Help is greatly appreciated.

  # Fork this repo
  # Add an issue to this repos issues with a link to you commit/compare-view
  # I'll get to it as soon as possible

### Breakdown

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
