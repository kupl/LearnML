(* Problem 1 *)
let rec fib : int -> int
= fun n -> 0 (* TODO *)
let rec fib n =
	match n with
	| 0->0
	| 1->1
	| _ -> fib(n-1)+fib(n-2)


(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0 (* TODO *)
let rec pascal (n1, n2) =
	if n1 = n2 then 1
	else if n2 =0 then 1
	else pascal(n1-1, n2-1)+pascal(n1-1,n2)
	

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true (* TODO *)
let rec prime n =
    let n = abs n in
    let rec divisor d =
    d * d > n || (n mod d <> 0 && divisor(d+1)) in
    n<>1 && divisor 2


(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 (* TODO *)
let rec sigma f a b =
	let rec sum f n =
	if n>1 then f n+ sum f (n-1)
	else if n=1 then 1
	else 0
in 
sum f b - sum f (a-1)

