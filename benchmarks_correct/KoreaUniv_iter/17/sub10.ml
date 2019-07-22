
(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> fun k -> let rec loop x =
if x = 0 then k
else f (loop (x-1)) 
in (loop n)
