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
handle_path(<<"POST">>, [<<"api">>, <<"calculator">>], Req, State) ->
    {ok, [{Post, _}], _} = cowboy_req:body_qs(Req),
    lager:debug("Calculator request for ~s", [Post]),
    Eqn = binary_to_list(Post),
    case is_minus(Post) of
        true -> lager:error("Unhandled operator minus request=~s", [Post]),
                {ok, Reply} = cowboy_req:reply(500, [{<<"Content-type">>, <<"text/plain">>}], [], Req),
                {ok, Reply, State};
	false -> Result = calc:rpn(Eqn),
                 {ok, Reply} = cowboy_req:reply(200, [{<<"Content-type">>, <<"text/plain">>}], num_as_binary(Result), Req),
                 {ok, Reply, State}
    end;

handle_path(_, _, Req, State) ->
    {ok, Reply} = cowboy_req:reply(401, [], [], Req),
    {ok, Reply, State}.

num_as_binary(Num) when is_integer(Num) -> integer_to_binary(Num);
num_as_binary(Num) -> io_lib:format("~p",[Num]).

is_minus(Bin) -> binary:match(Bin, [<<"-">>], []) =/= nomatch.
