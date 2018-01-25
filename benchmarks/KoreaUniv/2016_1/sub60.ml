(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n = 0 || n = 1 then n else fib (n-1) + fib (n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if (n2 = 0) || (n1 = n2) then 1 else pascal (n1-1, n2-1) + pascal (n1-1, n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> 
		let k = abs n in
		let rec not_divided_by d =
			d * d > k || (k mod d <> 0 && not_divided_by (d+1)) in
	k <> 1 && not_divided_by 2

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if b = a then f a else f b + sigma f a (b-1)

