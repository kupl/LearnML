let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->
  match n with
    | 0 -> fun x -> x
    | 1 -> f
    | _ -> fun x -> f (iter(n-1, f) x)
