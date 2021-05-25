let rec iter : (int * (int -> int)) -> int -> int = fun (n,f) ->
 match n with
 | 0 -> (function x -> x)
 | n -> (function x -> f (iter (n-1,f) x))