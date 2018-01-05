-module(metrix).

-author('Maxim Molchanov <mr.elzor@gmail.com>').

-export([put_value/2,
         get_value/1,
         get_value/2,
         increase_counter/1,
         decrease_counter/1,
         increase_counter/2,
         decrease_counter/2,
         get_pure_node/1]).


put_value(Name, _Value) when not is_list(Name) ->
    {error, bad_arg_type};

put_value(Name, Value) ->
    metrix_collector:put(Name, Value).

increase_counter(Name) ->
    metrix_collector:increase(Name).

increase_counter(Name, Number) when (Number > 0) and (is_integer(Number)) ->
    metrix_collector:increase(Name, Number);
increase_counter(_Name, 0) ->
    ok.

decrease_counter(Name) ->
    metrix_collector:decrease(Name).

decrease_counter(Name, Number) when (Number > 0) and (is_integer(Number)) ->
    metrix_collector:decrease(Name, Number);
decrease_counter(_Name, 0) ->
    ok.

get_value(Name) ->
    get_value(Name, 5000).

get_value(Name, Timeout) ->
    metrix_collector:get(Name),
    receive
        {metrix, Name, Value} -> Value
        after Timeout -> undefined
    end.


get_pure_node(Node) ->
    Pred = fun
        ($@) -> false;
        (_C) -> true
    end,
    NodeStr = atom_to_list(Node),
    lists:takewhile(Pred, NodeStr).
