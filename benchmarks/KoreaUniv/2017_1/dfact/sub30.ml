let iseven x = (x mod 2) = 0
exception Input_range

(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
	if a > b then raise Input_range
	else if a = b then f a
	else (f a)*(product f (a+1) b)

(* problem 5*)

let dfact : int -> int
= fun n -> 
	if n =0 then 1
	else if iseven n then product (fun x->2*x) 1 (n/2)
	else product (fun x ->((2*x)-1)) 1 ((n+1)/2)