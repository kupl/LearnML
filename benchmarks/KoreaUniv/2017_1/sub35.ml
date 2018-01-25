(*
	Author: Jacob Pihl
	Date: 07 Oct 2017
	Description: Homework 1 for Programming Languages
*)

(* Helper functions *)
let is_even n =
	n mod 2 = 0

let rec nth l n =
match l with
	[] -> 0
	|a::t -> if n=0 then a else nth t (n-1)

(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
match n with
	0 -> 1
	|1 -> b
	|_ -> if (is_even n) then
			(fastexpt b (n/2))*(fastexpt b (n/2))
		else 
			b*(fastexpt b (n-1))

(* problem 2*)
let rec smallest_divisor : int -> int
= fun n ->
if (is_even n) then 	(* Number is even *)
	2
else
	let rec loop n d =
		if n mod d = 0 then d 	(* Number divisible by d *)
		else if d*d > n then n 	(* Number is prime *)
		else loop n (d+2) 		(* Do the ugly loop *)
	in loop n 3


(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
match n with
	0 -> fun x -> x
	|_ -> fun x -> iter(n-1, f) (f x)

(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
if a = b then f a
else (product f (a+1) b)*(f a)

(* problem 5*)
let rec dfact : int -> int
= fun n ->
if is_even n then (fastexpt 2 (n/2)) * (product (fun x->x) 1 (n/2))
else (product (fun x->x) 1 n) / (dfact (n-1))

(* problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n ->
match l with
	[] -> []
	|h::t -> if n=0 then l else drop t (n-1)

(* problem 7*)
let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
match lst with
	[] -> ([], [])
	|(a,b)::t -> let (at, bt) = unzip t in (a::at, b::bt)

(* problem 8*)
let rec change : int list -> int -> int
= fun coins amount ->
	if amount = 0 then 1
	else if amount < 0 then 0
	else if coins = [] then 0
	else (change coins (amount - (nth coins 0))) + (change (drop coins 1) amount)