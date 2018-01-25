(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> let rec loop x =
if x > b then 1
else (f x)*(loop (x+1))
in (loop a)

(* problem 5*)

let dfact : int -> int
= fun n -> if n mod 2 = 0 then product (fun x->2*x) 1 (n/2)
else product (fun x->2*x-1) 1 ((n+1)/2)