-module(calculator).

-export([
         start/0,
         stop/0
        ]).

%% @spec start() -> ok
%% @doc Start the calculator server.
start()             -> [application:start(App) || App <-
                                        [ranch, cowboy,
                                         protobuffs, inets,
                                         webmachine, calculator
                                         ]].

%% @spec stop() -> ok
%% @doc Stop the calculator server.
stop() ->
    application:stop(calculator).
