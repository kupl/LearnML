(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> if n=0 then 1
	else if n mod 2 = 0 then (fastexpt b (n/2)) * (fastexpt b (n/2))
	else b * (fastexpt b (n-1))


(* problem 2*)
let smallest_divisor : int -> int
= fun n -> let rec loop i =
	if i*i > n then n
	else if (n mod i = 0) then i
	else loop (i+1)
	in loop 2


(* problem 3*)
let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> let rec loop n x =
	if n=0 then x
	else loop (n-1) (f x)
	
	in loop n


(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a=b then f a
	else (f a) * (product f (a+1) b)


(* problem 5*) 
let dfact : int -> int
= fun n -> product (fun x -> (2*x - n mod 2)) 1 ((n+1)/2)


(* problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n -> if n=0 then l
	else match l with 
	| [] -> []
	| hd::tl -> drop tl (n-1)


(* problem 7*)
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let rec loop listA listB lst =
	match lst with
	| [] -> (listA, listB)
	| hd::tl -> match hd with
		(x, y) -> loop (listA @ [x]) (listB @ [y]) tl
	in loop [] [] lst


(* problem 8*)
let rec change : int list -> int -> int
= fun coins amount -> if amount=0 then 1
	else match coins with
	| [] -> 0
	| hd::tl -> if hd<=amount then (change coins (amount-hd)) + (change tl amount)
	else change tl amount