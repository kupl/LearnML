let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->
match n with
    | 0 -> (function x -> x)
    | count -> (function x -> f (iter(count-1,f) x));;
