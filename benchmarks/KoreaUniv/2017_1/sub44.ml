(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> match n with
			|0 -> 1
			|_ -> match n mod 2 with
				|1 -> b* (fastexpt b ((n-1)/2))*(fastexpt b ((n-1)/2)) 
				|_ -> (fastexpt b (n/2))*(fastexpt b (n/2));;
			 	
(* problem 2*)

let rec smallest_divisor : int -> int
= fun n ->
	let m=2 in
		let rec for_loop n m = if n<m*m then n else if n mod m = 0 then m else for_loop n (m+1) in for_loop n m;;

(* problem 3*)

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> match n with
				|0 -> fun x -> x
				|_ -> fun x -> f (iter(n-1,f) x);;

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a>b then 1 else ((f a) * (product f (a+1) b));;

(* problem 5*)

let dfact : int -> int
= fun n -> match n mod 2 with
		|0 -> product (fun x->2*x) 1 (n/2)
		|_ -> product (fun x->2*x-1) 1 ((n+1)/2);;

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n -> match l with
			| [] -> []
			| hd::tl -> match n with
						|0 -> hd::tl
						|_ -> drop tl (n-1);;

(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->let rec recur first second lst = match lst with
				| [] -> (first, second)
				| hd::tl -> let (x,y)=hd in
							let first = first@[x] in
							let second = second@[y] in
								recur first second tl in
			recur [] [] lst;;

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> match coins with
					| [] -> 0
					| hd::tl -> if amount<0 then 0 else
								if amount=0 then 0 else
if (amount-hd) = 0 then (1+ (change tl (amount-hd)))
								else if (amount-hd) > 0 then (change coins (amount-hd) + change tl amount)
								else change tl amount;;