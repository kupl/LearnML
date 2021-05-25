(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) x ->
 match n with
 | 0 -> x
 |_ -> f(iter(n-1, f) x ) 