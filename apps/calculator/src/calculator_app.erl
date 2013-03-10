-module(calculator_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1,
         init/1]).

-define(MAX_RESTART, infinity).
-define(MAX_TIME,    infinity).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State) -> ok.

init([]) ->
    lager:info("Starting http listener..."),
    {ok, Http_Listen_Port} = application:get_env(calculator, http_listen_port),
    Http_Dispatch = [{'_', [
                            {[<<"api">>, '_'],           http_calculator_handler, []}
                           ]}],
    cowboy:start_http(http_handler, 10, [{port, Http_Listen_Port}], [{dispatch, Http_Dispatch}, {poolsize,10}]),

    lager:info("Starting ibrowse"),
    application:start(ibrowse),

    lager:info("Sending source code mapping"),
    calculator_util:post_mapping(),

    lager:info("Done!\n"),

    Children = [
                  {calculator_server,    {calculator_server,    start_link, []}, permanent, 5000, worker, [calculator_server]}
               ],

    {ok, { {one_for_one, 10000, 10}, Children} };

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              {   undefined,
                  {Module,start_link,[]},
                  temporary,
                  2000,
                  worker,
                  []
              }
            ]
        }
    }.
