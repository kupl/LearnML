(*problem 3*)
let iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
let rec ite n f =
if n = 0 then fun x -> x
else fun x -> ite (n-1) f (f x) in ite n f;;
