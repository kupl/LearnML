let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
 
let id_func x = x
 
in
 
let compose f g x = f (g x)
 
in
 
let rec iter_fun n f =
if n = 0 then id_func
else compose f (iter_fun (n-1) f)
 
in
 
if n = 0 then id_func
else if n < 0 then raise (Failure "n is negative value")
else iter_fun n f