(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
	 	if n =0 then 1
		else if n<0 then raise (Failure "n should be a positive integer")
		else if (n mod 2)=0 then (fastexpt b (n/2))*(fastexpt  b (n/2))
		else b*(fastexpt b (n-1));;	

(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
	 let rec div n d = if (d*d)>n then n
										 else if (n mod d)=0 then d
  			  					 else div n (d+1)
   in div n 2;;



(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n=0 then (fun x -> x)
  						else if n=1 then (fun x -> f x)
  						else (fun x -> f(iter (n-1,f) x));; 


(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a=b then f(b)
  						 else if a>b then raise (Failure "b should be larger than a")
							 else f(a) * (product f (a+1) b);;

(* problem 5*)

let dfact : int -> int
= fun n -> if (n mod 2)=0 then product (fun x -> 2*x) 1 (n/2) 
  				 else product (fun x -> (2*x)-1) 1 ((n+1)/2);;

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> if n=0 then l 
  					 else if n<0 then raise (Failure "n should be larger than 0")
						 else match l with 
  								| [] -> []
  								| hd::tl -> drop tl (n-1);;


(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
	match lst with
	| [] -> ([],[])
	| hd::tl -> match hd with
							| (a,b) -> (a::(fun (x,_) -> x) (unzip tl),b::(fun (_,x) -> x) (unzip tl));;




(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> 
	let rec reverse = fun l ->
		match l with 
		| [] -> []
		| hd::tl -> (reverse tl)@[hd]
	in if amount = 0 then 1
	else if amount < 0 then 0
	else match (reverse coins) with
			| [] -> 0
			| hd::tl -> (change coins (amount-hd)) + (change tl amount);;

