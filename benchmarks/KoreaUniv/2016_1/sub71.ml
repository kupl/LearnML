(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with  (* TODO *)
0 -> 0
| 1 -> 1
|_ -> fib(n-1) + fib(n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> (* TODO *)
if n1 < n2 then 0
else if n2 = 0 then 1
else if n1 = n2 then 1
else pascal (n1-1,n2-1) + pascal (n1-1,n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true (* TODO *)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 (* TODO *)
