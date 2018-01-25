(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> 
	if n = 0 then 1
	else
		if n = 1 then b
		else    if (n mod 2) = 1 then (fastexpt b 1) * (fastexpt b (n-1))
			else (fastexpt b (n/2)) * (fastexpt b (n/2))

(* problem 2*)

let rec compare d n = 
	if (n mod d) = 0 then d 
	else (if (d*d) > n then n else (compare (d+2) n))

let smallest_divisor : int -> int
= fun n ->
	if (n mod 2) = 0 then 2
	else (compare 3 n)

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> fun x -> if n = 1 then f x
	       else if n < 1 then raise (Failure "ERROR")
	       else f((iter(n-1,f)) x)

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f b
	       else (f a) * (product f (a+1) b)

(* problem 5*)

let dfact : int -> int
= fun n -> if (n mod 2) = 0 then product (fun x -> 2*x) 1 (n/2)
	   else product (fun x -> 2*x-1) 1 ((n+1)/2)

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> match l with
		| [] -> []
		| hd::tl -> if n = 0 then l else drop tl (n-1)

(* problem 7*)

let rec unzip_x l = 
	match l with
	| [] -> []
	| hd::tl -> match hd with (x,_) -> [x]@unzip_x tl

let rec unzip_y l = 
	match l with
	| [] -> []
	| hd::tl -> match hd with (_,y) -> [y]@unzip_y tl

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> ((unzip_x lst), (unzip_y lst))

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> if amount < 0 then 0
		      else if amount = 0 then 1 else match coins with
					| [] -> 0
					| hd::tl -> if hd = amount then 1 + (change tl amount) 
						    else (change coins (amount-hd)) + (change tl amount)

