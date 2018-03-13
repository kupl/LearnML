(* problem 3*)
let iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
if n=0 then fun x->x