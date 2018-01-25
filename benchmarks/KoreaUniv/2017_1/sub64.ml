(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
	if n <= 0 then (* last step *)
		1
	else if n == 1 then (* b powered by 1 *)
		b
	else if (n mod 2) == 1 then (* odd number *)
		b * (fastexpt b (n - 1))
	else (* even number *)
		(fastexpt b (n / 2)) * (fastexpt b (n / 2))
;;
(*
print_endline(string_of_int(fastexpt 2 0));;
print_endline(string_of_int(fastexpt 2 1));;
print_endline(string_of_int(fastexpt 2 7));;
print_endline(string_of_int(fastexpt 2 8));;
print_endline(string_of_int(fastexpt 3 3));;
print_endline(string_of_int(fastexpt 3 4));;
*)

(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
	if n mod 2 == 0 then 2
	else
	(
		let rec div
		= fun sqr n cur ->
		(
			if sqr < cur then n (* if there is no divisor smaller than (sqrt n), return original n *)
			else if n mod cur == 0 then cur (* if n is divisible by cur, return cur *)
			else div sqr n (cur+1)
		)
		in div (int_of_float (sqrt (float_of_int n))) n 2
	)
;;
(*
print_endline(string_of_int(smallest_divisor 8));;
print_endline(string_of_int(smallest_divisor 27));;
print_endline(string_of_int(smallest_divisor 121));;
print_endline(string_of_int(smallest_divisor 141));;
print_endline(string_of_int(smallest_divisor 199));;
*)

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
	if n <= 0 then (fun x -> x)
	else (fun x -> (iter (n-1, f)) (f x))
;;
(*
let test3 = iter (3, fun x -> 2 + x) in
print_endline(string_of_int(a 0));;
*)

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
	if a >= b then (f a)
	else (f a) * (product f (a+1) b)
;;
(*
let test4 = product (fun x -> x) in
print_endline (string_of_int (test4 1 5));;
let test4_2 = product (fun x -> x + 1) in
print_endline (string_of_int (test4_2 2 4));;
*)

(* problem 5*)

let dfact : int -> int
= fun n -> 
	if (n mod 2) == 0 then (* n is even, ignore the odd x values *)
		product (fun x -> if x mod 2 == 1 then 1 else x) 1 n
	else (* n is odd, ignore the even x values *)
		product (fun x -> if x mod 2 == 0 then 1 else x) 1 n

;;
(*
print_endline (string_of_int (dfact 7));;
print_endline (string_of_int (dfact 8));;
*)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
	if n = 0 then l
	else if n < 0 then raise (Failure "Negative index access error")
	else 
	(
		match l with
		[] -> []
		| hd::tl -> drop tl (n-1)
	)
;;
(*
print_string (String.concat " " (List.map string_of_int (drop [1;2;3;4;5] 2)));;
*)

(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
	match lst with
	[] -> ([], [])
	| hd::tl -> 
		let (a, b) = hd in
		let unzip_tail = unzip tl in
		let (alist, blist) = unzip_tail in
		(a::alist, b::blist)
;;
(*
let unzipped = unzip [(1, 10);(2, 20);(3, 30);(4, 40)] in
match unzipped with
(alist, blist) ->
	print_string (String.concat " " (List.map string_of_int (alist)));
	print_string (String.concat " " (List.map string_of_int (blist)));;
*)

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> 
	if amount < 0 then 0 (* if amount is negative, return 0 *)
	else if amount == 0 then 1 (* if amount is zero, return 1 *)
	else if (List.length coins) == 1 then (* if coins is a singleton list, if amount is divisible by the coin, return 1 else 0 *)
	(
		match coins with
		| [] -> 0
		| hd::tl -> 
			if (amount mod hd) == 0 then 1 else 0
	)
	else
	(
		let coins_desc = List.rev (List.sort compare coins) in (* coins in descending order *)
		match coins_desc with
		[] -> 0
		| hd::tl ->
		(
			let result = ref 0 in
			for i = 0 to amount / hd do
				result := !result + (change tl (amount - (hd * i)))
			done;
			!result
		)		
	)
;;
(*
print_endline (string_of_int (change [1;5;10] 12));;
print_endline (string_of_int (change [1;5;10;25;50] 100));;
print_endline (string_of_int (change [1;5;10] 0));;
print_endline (string_of_int (change [1;5;10] (-5)));;
print_endline (string_of_int (change [] 10));;
print_endline (string_of_int (change [3] 10));;
*)