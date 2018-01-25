(* problem 1 완료*)
let rec fastexpt : int -> int -> int
= fun b n -> if n = 0 then 1
else if (n mod 2 = 0) then (fastexpt b (n/2))*(fastexpt b (n/2))
else b*(fastexpt b (n-1))
;;

(* problem 2 *)

let smallest_divisor : int -> int
= fun n -> 
if n = 1 then 1	
else let i = 2 in
	let rec checkundersquare a b = 
		if a >= (b*b) then
			if (a mod b) = 0 then b	
			else checkundersquare a (b+1)
		else a
	in checkundersquare n i
;;

(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int) 
= fun (n,f) -> 
if n = 0 then (fun x-> x)
else fun x -> f( iter(n-1, f)(x) )
;;

(* problem 4 완료 *)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f a
else (f a)*(product f (a+1) b)
;;


(* problem 5 *)
let dfact : int -> int
= fun n -> 
if n mod 2 = 1 then product (fun x->((2*x)+1)) 1 ((n-1)/2)
else product (fun x -> (x*2)) 1 (n/2);;



(*
let rec dfact : int -> int
= fun n -> if n = 1 then 1
else if n = 2 then 2
else n * (dfact (n-2))
;;
*)
(* problem 6 완료 *)

let rec drop : 'a list -> int -> 'a list
= fun l n -> 
match l with
 [] -> l
| hd::tl -> if n>=1 then drop tl (n-1) else l
;;

(* problem 7 완료 *)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
match lst with 
 [] -> ([], [])
| hd :: tl -> 
	let fst (x,_) = x 
		in let snd (_,x) = x 
			in ((fst hd)::(fst (unzip tl)), (snd hd)::(snd (unzip tl)))
;;



(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> 
	if amount < 0 then 0
	else
		match coins with
		 [] -> 0
		| hd::tl ->
		if amount = 0 then	(1 + (change tl (amount - hd)))
		else (change tl amount) + (change coins (amount - hd))
;;