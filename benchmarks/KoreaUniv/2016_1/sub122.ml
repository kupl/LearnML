(* Problem 1 *)
let rec fib : int -> int
= fun n -> 0

let rec fib (x) =
	if (x = 0) then 0
	else if (x = 1) then 1
	else (fib (x-1)) +  (fib (x-2));;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 0;;

let rec pascal (x, y) =
	if y = 0 then 1
	else if x = y then 1
	else if x < y then raise (Failure "x >= y value")
	else pascal (x-1, y) + pascal (x-1, y-1);;


(* Problem 3 *)
let rec prime : int -> bool
= fun n -> true;;

let prime (x) =
	let rec isPrime x i =
		if i*i > x then false
		else if x mod i = 0 then true
		else isPrime x (i+1)
	in
		if x <= 1 then false
		else not (isPrime x 2);;
	

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 0 (* TODO *)


let rec sigma f a b =
	if a < b then f a + sigma f (a+1) b
	else if a = b then f a
	else f a + sigma f (a-1) b;;

