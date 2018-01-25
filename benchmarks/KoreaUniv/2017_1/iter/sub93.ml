
(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n=0 then f
else fun x -> iter(n-1 f)(f x);;
