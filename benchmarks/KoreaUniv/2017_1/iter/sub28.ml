(*problem 3. *)

let rec iter : int * (int->int) -> (int->int)
= fun(n,f) ->

let rec iter_f f g x = 
f(g x)

in
if n = 0 then (fun x -> x)
else iter_f f (iter (n-1,f)) 