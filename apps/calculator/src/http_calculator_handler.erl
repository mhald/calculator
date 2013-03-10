%%
%% Copyright 2012
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(http_calculator_handler).
-author('mhald@mac.com').

-behavior(cowboy_http_handler).

-export([init/3,
         handle/2,
         terminate/2]).

init({_Any, http}, Req, _) -> {ok, Req, undefined_state}.

handle(Req, State) ->
    {Path, Req1}   = cowboy_req:path(Req),
    Path_Parts     = lists:filter(fun(<<>>) -> false; (_) -> true end, binary:split(Path, <<"/">>, [global])),
    {Method, Req2} = cowboy_req:method(Req1),
    handle_path(Method, Path_Parts, Req2, State).

terminate(_Req, _State) -> ok.

%% example.loggingwithpopcorn.com/api/calculator?x=5&y=2&function=*
handle_path(<<"GET">>, [<<"api">>, <<"calculator">>], Req, State) ->
    {ok, Post, Req2} = cowboy_req:body(Req),
    {X, _} = cowboy_req:qs_val(<<"x">>, Req),
    {Y, _} = cowboy_req:qs_val(<<"y">>, Req),
    {Function, _} = cowboy_req:qs_val(<<"function">>, Req),
    Result = calculate(binary_to_integer(X), Function, binary_to_integer(Y)),
    {ok, Reply} = cowboy_req:reply(200, [{<<"Content-type">>, <<"text/plain">>}], int_to_binary(Result), Req2),
    {ok, Reply, State};

handle_path(_, _, Req, State) ->
    {ok, Reply} = cowboy_req:reply(401, [], [], Req),
    {ok, Reply, State}.

int_to_binary(Value) -> list_to_binary(integer_to_list(Value)).
binary_to_integer(Value) -> list_to_integer(binary_to_list(Value)).
calculate(X, <<"+">>, Y) -> X + Y;
calculate(X, <<"-">>, Y) -> X - Y;
calculate(X, <<"*">>, Y) -> X * Y;
calculate(X, <<"/">>, Y) -> X / Y.
