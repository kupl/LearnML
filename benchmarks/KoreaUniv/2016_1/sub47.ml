(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n = 0 then 0 else if n = 1 then 1 else (fib(n-1) + fib(n-2));;
 

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0 (* TODO *)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true (* TODO *)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 (* TODO *)

