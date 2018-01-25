(* Problem 1 *)
let rec fib : int -> int
= fun n -> 0 
  if n < 3 then
 	1
 	else
	fibonacci (n-1) + fibonacci (n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0 


(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true (* TODO *)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 (* TODO *)

