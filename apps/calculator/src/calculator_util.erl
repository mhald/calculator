-module(calculator_util).
-author('mhald@mac.com').

-export([post_mapping/0]).

post_mapping() ->
    post_mapping_from_file(file:read_file(code:priv_dir(calculator) ++ "/calculator_git.json")),
    ok.

post_mapping_from_file({error,enoent}) -> ok;
post_mapping_from_file({ok, Binary}) ->
    Host = application:get_env(calculator, popcorn_host),
    Port = application:get_env(calculator, popcorn_port),
    post_mapping_from_file_to_server({ok, Binary}, Host, Port),
    ok.

post_mapping_from_file_to_server({ok, Binary}, {ok, Host}, {ok, Port}) ->
    ibrowse:send_req("http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/api/mapping", [], post, Binary);
post_mapping_from_file_to_server(_, _, _) -> ok.
