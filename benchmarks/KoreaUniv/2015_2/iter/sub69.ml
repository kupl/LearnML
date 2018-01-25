let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> match n with
0 -> fun x -> x
|_ -> if n<0 then raise(Failure "error") else
fun x -> f(iter(n-1,f) x);;
