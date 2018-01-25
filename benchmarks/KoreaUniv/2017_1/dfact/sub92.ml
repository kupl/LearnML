(* problem 4-solve*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if b=a then f b else f a*product f (a+1) b

(* problem 5-solve*)

let rec dfact : int -> int
= fun n -> let rec product f a b = if b=a then f b else f a*product f (a+2) b in
		if n mod 2 = 0 then 
		product (fun x->x) 2 n else product (fun x->x) 1 n 