(* problem 3*)

let rec iter : int*(int->int)->(int->int)
 =fun(n, f) a ->
 if n=0 then a
 else if n=1 then f(a)
 else iter(n-1, f) (iter(1, f) a);;
