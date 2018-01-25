(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> if n=0 then 1
	     else
		if n mod 2 = 0 then let x = (fastexpt b (n/2)) in x*x
		else b*(fastexpt b (n-1));;


(* problem 2*)
let rec divide : int -> int -> int
= fun n i -> if n mod i = 0 then (n/i)
	     else if (i * i) <= n then n
		  else divide n (i-1);;

let smallest_divisor : int -> int
= fun n -> divide n (n-1);;


(* problem 3*)
let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> if n = 1 then f else fun x -> (f (iter((n-1), f) x));;


(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a>b then 0
	       else if a==b then (f a)
		    else ((f a) * (product f (a+1) b));;


(* problem 5*)
let rec dfact : int -> int
= fun n -> if n<=2 then n
	   else n * (dfact (n-2));;


(* problem 6*)
let rec drop : 'a list -> int -> 'a list
= fun l n -> if n = 0 then l
	     else match l with
			| [] -> []
			| hd::tl -> (drop tl (n-1));;
		

(* problem 7*)
let get_first
= fun l -> match l with
		| (la, _) -> la;;

let get_second
= fun l -> match l with
		| (_, lb) -> lb;;

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> match lst with
		| [] -> ([],[])
		| (x,y)::tl -> let res = (unzip tl) in (x::(get_first res), y::(get_second res));;


(* problem 8*)
let rec change : int list -> int -> int
= fun coins amount -> if amount=0 then 1
		      else if amount<0 then 0
		      	   else match coins with
				| [] -> 0
				| hd::tl -> if amount < hd then (change tl amount)
					    else let rec sum last_amount = if last_amount<hd then 0
									   else (sum (last_amount-hd)) + (change tl (last_amount-hd))
						 in (change tl amount) + (sum amount);;
