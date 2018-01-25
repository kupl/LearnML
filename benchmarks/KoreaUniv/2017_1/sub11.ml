(* problem 1*)
let fastexpt : int -> int -> int
= fun b n ->
	let rec help b n =
		if n == 0 then 1
		else if n mod 2 == 0 then (help b (n/2)) * (help b (n/2))
		else b * (help b (n-1)) in
			help b n

(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
	let m = sqrt(float_of_int n) in
		let x = int_of_float m in
    		let rec help n i = 
        		if i > x then n 
    			else if n mod i == 0 then i 
    			else help n (i + 1) in
    				help n 2;;

(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
	let idf n = n in
		let rec help n f =
			if n == 0 then idf
			else if n == 1 then f
			else f (*help (n-1) f f*) in
				help n f



(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> 
	let rec help f a b x =
		if a > b then x
		else if a == b then x*(f a)
		else help f (a+1) b (x*(f a)) in
			help f a b 1;;


(* problem 5*)

let dfact : int -> int
= fun n -> 
	if n mod 2 == 0 then product (fun x-> 2*x) 1 (n/2)
	else product (fun x-> 2*x-1) 1 ((n+1)/2)


(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> 
	let rec help l n =
		if List.length l <= n then []
		else if n == 0 then l
		else (help (match l with 
					|[] -> []
					|h::t -> t) 
						(n-1)) in
			help l n

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (*TODO*)
	let rec first lst =
		match lst with
		|[] -> []
		|(a,b)::t -> a::(first t) in
			let rec last lst =
				match lst with
				|[] -> []
				|(a,b)::t -> b::(last t) in
					(first lst, last lst)


(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> (*TODO*)
	let rec help c m a =
		if a == 0 then 1
		else if a < 0 then 0
		else if c == [] then 0
		else if (m <= 0 && a >= 1) then 1
		else (help c (m-1) a + help c m (a-(List.nth c ((List.length c)-1)))) in
			help coins (List.length coins) amount

