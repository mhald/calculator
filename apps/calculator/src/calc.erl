-module(calc).
-export([rpn/1, rpn_test/0, sum_stack/1]).
 
rpn(L) when is_list(L) ->
    [Result] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Result.
 
%% rpn(Op, Stack)
rpn("+", [Num1, Num2|Stack])    -> [Num2 + Num1|Stack];
rpn("-", [Num1, Num2|Stack])    -> [Num2 - Num1|Stack];
rpn("*", [Num1, Num2|Stack])    -> [Num2 * Num1|Stack];
rpn("/", [Num1, Num2|Stack])    -> [Num2 / Num1|Stack];
rpn("^", [Num1, Num2|Stack])    -> [math:pow(Num2, Num1)|Stack];
rpn("ln", [Num|Stack])          -> [math:log(Num)|Stack];
rpn("log10", [Num|Stack])       -> [math:log10(Num)|Stack];
rpn("sum", [Num1, Num2|Stack])  -> sum_stack([Num2 + Num1|Stack]);
rpn("prod", [Num1, Num2|Stack]) -> prod_stack([Num2 * Num1|Stack]);
 
rpn(X, Stack)                   -> [read(X)|Stack].
 
%% For reading integers
read(Num) ->
    case string:to_float(Num) of
    {error, no_float} -> list_to_integer(Num);
    {F,_} -> F
    end.
 
 
%% Base case: Num1 or Num2 is not a number or empty stack
%% else add/mult Num1 and Num2 and push on stack
%% these are foldls
sum_stack([Num1, Num2|Stack])  -> sum_stack([Num2 + Num1|Stack]);
sum_stack([Num])               -> [Num];
sum_stack([])                  -> [0].
 
prod_stack([Num1, Num2|Stack]) -> prod_stack([Num2 * Num1|Stack]);
prod_stack([Num])              -> [Num];
prod_stack([])                 -> [0].
 
 
rpn_test() ->
    5 = rpn("2 3 +"),
    87 = rpn("90 3 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    ok = try
             rpn("90 34 12 33 55 66 + * - +")
         catch
             error:{badmatch,[_|_]} ->
             ok
         end,
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    8.0 = rpn("2 3 ^"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    true = math:log(2.7) == rpn("2.7 ln"),
    true = math:log10(2.7) == rpn("2.7 log10"),
    [0] = sum_stack([]),
    [10] = sum_stack([10]),
    [20] = sum_stack([10, 10]),
    [50] = sum_stack([10, 10, 10, 20]),
    50 = rpn("10 10 10 20 sum"),
    10.0 = rpn("10 10 10 20 sum 5 /"),
    1000.0 = rpn("10 10 20 0.5 prod"),
    ok.
