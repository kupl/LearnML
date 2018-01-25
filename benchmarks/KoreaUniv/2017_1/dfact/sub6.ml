(* problem 5*)

let dfact : int -> int
= fun n ->
let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
if a==b then f b
else (f b) * (product f a (b-1)) in
if (n mod 2)==0 then product (fun x->2*x) 1 (n/2)
else product (fun x->2*x-1) 1 ((n+1)/2);;