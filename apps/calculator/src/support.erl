-module(support).

-export([
        info/1,
        warn/1,
        error/1, 
        log/2,
        top/0, top/1, top/2,
        ftop/0, ftop/1, ftop/2,
        generate_fake_logs/1,
        generate_fake_logs/2
       ]).

-spec top() -> [tuple()].
top() -> top(10).

-spec top(pos_integer()) -> [tuple()].
top(Length) -> top(Length, mq).

-type top_field() :: function | fn | name | nm | messages | mq | reductions | rd | psize | ps.
-spec top(pos_integer(), top_field()) -> [tuple()].
top(Length, Sort) ->
    lager:info("Gathering Process List..."),
    {Time, Procs} =
        timer:tc(
            fun() ->
                lists:sublist(
                    lists:reverse(
                        lists:sort(
                            [{top_field(Sort,P), P}
                             || P <- pman_process:r_processes(node()),
                                not pman_process:is_system_process(P)])),
                        Length)
            end),
    lager:info("Process List gathered in ~~~p secs", [round(Time/1000000)]),
    [{Pid,
      [{nm, top_field(nm, Pid)},
       {mq, top_field(mq, Pid)},
       {rd, top_field(rd, Pid)},
       {ps, top_field(ps, Pid)},
       {fn, top_field(fn, Pid)}]} || {_, Pid} <- Procs].

top_field(mq, P) -> pman_process:msg(P);
top_field(rd, P) -> pman_process:reds(P);
top_field(ps, P) -> pman_process:psize(P);
top_field(nm, P) -> pman_process:get_name(P);
top_field(fn, P) -> pman_process:function_info(P);
top_field(messags, P)   -> pman_process:msg(P);
top_field(reductions, P)-> pman_process:reds(P);
top_field(psize, P)     -> pman_process:psize(P);
top_field(name, P)      -> pman_process:get_name(P);
top_field(function, P)  -> pman_process:function_info(P).

-spec ftop() -> ok.
ftop() -> ftop(10).

-spec ftop(pos_integer()) -> ok.
ftop(Length) -> ftop(Length, mq).

-spec ftop(pos_integer(), top_field()) -> ok.
ftop(Length, Sort) ->
    Data = top(Length, Sort),
    io:format("~-15s | ~-25s | ~-7s | ~-7s | ~-7s | ~s~n", [pid, name, msgq, reds, psize, function]),
    io:format("~100c~n", [$-]),
    lists:foreach(
        fun({Pid, Fields}) -> io:format("~15s | ~-25s | ~7w | ~7w | ~7w | ~w~n", [pid_to_list(Pid) | [F || {_, F} <- Fields]]) end,
        Data).

info(Msg) -> lager:info(Msg, []).
warn(Msg) -> lager:warning(Msg, []).
error(Msg) -> lager:error(Msg, []).

log(Level, Msg) -> lager:log(Level, [{module,    ?MODULE},
                                     {function,  log},
                                     {line,      ?LINE},
                                     {pid,       self()},
                                     {node,      node()}], Msg).

generate_fake_logs(Count) ->
  Severities = lists:seq(1, 8),
  lists:foreach(fun(_) ->
      Random_Severity = lists:nth(random:uniform(length(Severities)), Severities),
      Message = "#random #fake log @~s",
      Args = [random_id()],
      case Random_Severity of
        1 -> lager:debug(Message, Args);
        2 -> lager:info(Message, Args);
        3 -> lager:notice(Message, Args);
        4 -> lager:warning(Message, Args);
        5 -> lager:error(Message, Args);
        6 -> lager:critical(Message, Args);
        7 -> lager:alert(Message, Args);
        8 -> lager:emergency(Message, Args)
     end,
     timer:sleep(random:uniform(10))
    end, lists:seq(1, Count)).

generate_fake_logs(Severity, Count) ->
    lists:foreach(fun(_) ->
        case Severity of
            debug -> lager:debug("#random #fake log @~s", [lio_support:random_iq_id()]);
            error -> lager:error("#random #fake log @~s", [lio_support:random_iq_id()])
        end
      end, lists:seq(1, Count)).


random_id()  -> Length = 6,
    AllowedChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    random:seed(now()),
    lists:foldl(fun(_, Acc) ->
        [lists:nth(random:uniform(length(AllowedChars)), AllowedChars)] ++ Acc
      end, [], lists:seq(1, Length)).
