-module(calculator_util).
-author('mhald@mac.com').

-export([post_mapping/0]).

post_mapping() ->
    post_mapping_from_file(file:read_file(code:priv_dir(calculator) ++ "/calculator_git.json")).

post_mapping_from_file({error,enoent}) -> missing_map_file;
post_mapping_from_file({ok, Binary}) ->
    Host = application:get_env(calculator, popcorn_host),
    Port = application:get_env(calculator, popcorn_port),
    post_mapping_from_file_to_server({ok, Binary}, Host, Port).

post_mapping_from_file_to_server({ok, Binary}, {ok, Host}, {ok, Port}) ->
    lager:debug("Sending up HTTP SVN mapping to ~p ~p", [Host, Port]),
    ibrowse:send_req("http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/api/mapping", [], post, Binary);
post_mapping_from_file_to_server(_, _, _) -> bad_popcorn_config.
