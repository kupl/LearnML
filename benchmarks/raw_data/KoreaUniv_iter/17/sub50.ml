(* problem 3*)

let id x = x;;
let compose f g = fun x -> f(g(x));;
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
if n = 0 then id
else if n = 1 then f
else compose f (iter(n-1,f));;