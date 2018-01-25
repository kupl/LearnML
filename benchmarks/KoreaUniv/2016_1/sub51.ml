(* 2013210045 양승호 *)

(* Skeleton *)
(* Problem 1 *)
let rec fib : int -> int
= fun n -> 0 (* TODO *)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0 (* TODO *)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true (* TODO *)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 (* TODO *)

(* My Code *)
(* Problem 1 *)
let rec fib n =
	match n with
	0 -> 0
	| 1 -> 1
	| _ -> fib(n-1) + fib(n-2)

(* Problem 2 *)
let rec pascal (a,b) =
	if a = 0 then 1
	else if b = 0 || b = a then 1
	else pascal(a-1,b-1) + pascal(a-1,b)

(* Problem 3 *)
let rec prime n =
  let rec div d = 
	if d = 1 then 1
	else if n mod d = 0 then -1
	else div(d-1) in
if div(n-1) = 1 then true
else false

(* Problem 4 *)
let rec sigma f a b =
	if a = b then f(b)
	else f(a) + sigma f (a+1) b
