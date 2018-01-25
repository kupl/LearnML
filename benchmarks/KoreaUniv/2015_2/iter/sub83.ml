let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->match n with 
|0 -> fun y->y
|1 -> f
|_ ->iter(n-1,fun x->f (f x) );;
