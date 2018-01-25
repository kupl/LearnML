(*problem 1*)
let rec fastexpt : int -> int -> int
 = fun b n ->
	if (n = 0) then 1
	else if (n mod 2 = 0) then ((fastexpt b (n/2)) * (fastexpt b (n/2)))
	else b * (fastexpt b (n-1))

(*problem 2*)
let rec sqrt : int -> int -> int
= fun n a ->
	if((a*a) > n) then n
	else if (n mod a = 0) then a
	else sqrt n (a+1)


let smallest_divisor : int -> int
= fun n ->
	if( n > 1) then sqrt n 2
	else if (n = 1) then 1
	else 0

(*problem 3*)
let rec comp (n, f) x =
	if(n = 0) then x
	else comp(n-1, f) (f x) 

let iter : int * (int -> int) -> (int -> int)
= fun (n, f) x ->
	comp(n, f) x

(*problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
	if(a = b) then a
	else b * (product f a (b-1))

(*problem 5*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
	if(a==b) then a
	else b * (product f a (b-1))

let rec fastexpt = fun b n ->
	if (n == 0) then 1
	else if (n mod 2 == 0) then ((fastexpt b (n/2)) * (fastexpt b (n/2)))
	else b * (fastexpt b (n-1))

let rec dfact : int -> int
	= fun n ->
	if((n mod 2) = 0) then (product (fun x -> x) 2 (n/2)) * fastexpt 2 (n/2)
	else (product (fun x -> x) 1 n)/(dfact (n-1))

(*problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n -> 
	match l with
		| [] -> []
		| hd::tl ->
			if(n = 0) then l
			else if(n = 1) then tl
			else drop tl (n-1)
			

(*problem 7*)
let rec split_first : ('a * 'b)list -> 'a list
= fun lst ->
	match lst with
		| [] -> []
		| (a, b)::tl -> 
			if(tl = []) then a::[]
			else a::(split_first tl)

let rec split_second : ('a * 'b)list -> 'b list
= fun lst ->
	match lst with
		| [] -> []
		| (a, b)::tl ->
			if(tl = []) then b::[]
			else b::(split_second tl)


let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
	if(lst = []) then ([], [])
	else (split_first lst, split_second lst)

(*problem 8*)
let rec length l =
	match l with
		| [] -> 0
		| h::t -> length t + 1

let head l =
	match l with
		| [] -> 0
		| h::t -> h

let tail l =
	match l with
		| [] -> []
		| h::t -> t

let rec change : int list -> int -> int
= fun coins amount ->
	if(amount = 0) then 1
	else if(amount < 0) then 0
	else if(length coins = 0) then 0
	else (change coins (amount - head coins)) + (change (tail coins) amount)
