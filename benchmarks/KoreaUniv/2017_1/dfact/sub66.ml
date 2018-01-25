(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a=b then f a
	else (f a) * (product f (a+1) b)


(* problem 5*) 
let dfact : int -> int
= fun n -> product (fun x -> (2*x - n mod 2)) 1 ((n+1)/2)