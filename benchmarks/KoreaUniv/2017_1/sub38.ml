(* problem 1*)

let rec fastexpt : int -> int -> int
= fun b n ->
	if n ==1 then b else
	if n mod 2 == 0 then
		let a = fastexpt b (n/2) in a*a
	else
		let a = fastexpt b (n/2) in a*a*b

(* problem 2*)

let smallest_divisor : int -> int
= fun n -> 
	let rec f a b =
		if a < b*b then a
		else if a mod b == 0 then b
		else f a (b+1)
	in f n 2

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
	if n == 1 then f
	else fun x -> (iter ((n-1),f)) (f x)

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
	if a == b then f a
	else (f a) * product f (a+1) b

(* problem 5*)

let dfact : int -> int
= fun n ->
	let rec f n a =
		if n == 1 || n == 2 then a*n
		else f (n-2) (a*n)
	in f n 1

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
	if n == 0 then l
	else match l with
	| [] -> []
	| hd::tl -> drop tl (n-1)

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
	let rec append l a =
		match l with
		| [] -> [a]
		| hd::tl -> hd::(append tl a)
	in let rec tuz t x y =
		match t with
		| [] -> (x,y)
		| (a,b)::tl -> tuz tl (append x a) (append y b)
	in tuz lst [] []

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
	if amount == 0 then 1
		else if amount < 0 then 0
		else match coins with
		| [] -> 0
		| hd::tl -> (change coins (amount-hd)) + change tl amount
