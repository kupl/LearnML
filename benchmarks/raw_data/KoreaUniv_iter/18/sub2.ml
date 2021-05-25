let rec iter : int * (int -> int) -> (int -> int) = fun (n,f) x->
  match n with
    | 0 -> f x
    | _ -> iter(n-1, f) (f x);;