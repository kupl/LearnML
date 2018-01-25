exception Input_out_of_domain_of_rule
(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n < 0 then raise Input_out_of_domain_of_rule else
	match n with 0 -> 0
	|1 -> 1
	|_ -> (fib (n-1)) + (fib (n-2)) (* TODO *)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if (n1 < 0 || n2 < 0 || n1 < n2) then raise Input_out_of_domain_of_rule else
	if (n1 == n2 || n2 == 0) then 1 else (pascal (n1-1, n2-1)) + (pascal (n1-1, n2)) (* TODO *)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> let n = abs n in
	if (n < 2) then false else (let rec sub_prime a = if(a == 1) then true else (if((a == 1) || (n mod a) == 0) then false else sub_prime (a-1)) in sub_prime (n-1)) (* TODO *)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a == b then (f a) else (if a < b then (sigma f a (b-1)) + f b else 0) (* TODO *)

