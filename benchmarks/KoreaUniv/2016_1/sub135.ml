
exception Problem

(* Problem 1 *)
let rec fib : int -> int
= fun n ->
		match n with
				0 -> 0
			| 1->1
			|_ -> fib(n-1) + fib(n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->
				if n1<0 then raise Problem
				else if n2<0 then raise Problem
				else if n1<n2 then raise Problem 
				else if n2 = 0 then 1
				else if n1 = n2 then 1
				else pascal (n1-1,n2-1) + pascal(n1-1,n2)

(* Problem 3 *)

let rec modul a b = 
		match b with
			2 -> a mod b <>0
			|_ -> a mod b <> 0 && modul a (b-1)


let rec prime : int -> bool
= fun n ->
			if n<2 then false else
			match n with
					2-> true
					|_ -> modul n (n-1)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
				if b>=a then (f a) + (sigma f (a+1) b) else 0

