exception Invalid;;

(* Problem 1 *)
let rec fib : int -> int
= fun n -> 
	match n with
		x when x < 0 -> raise Invalid|
		0 -> 0|
		1 -> 1|
		_ -> fib(n-1) + fib(n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 
	if n1 < 0 || n2 < 0 || n2 > n1 then raise Invalid
	else
		match n2 with
			0 -> 1|
			x when x = n1-> 1|
			_ -> pascal((n1-1),(n2-1)) + pascal((n1-1), (n2));;

(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
	if (abs n) = 1 then false
	else if n = 0 then raise Invalid
	else let  m = (abs n) in
		let rec is_divided d =
			if (d*d > m) then true
			else if (m mod d = 0) then false
			else is_divided (d+1) in
			 is_divided 2;;


(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
	if a > b then raise Invalid
	else
		match b with
			x when x = a -> (f a)|
			_ -> (f b) + (sigma f a (b-1));;
