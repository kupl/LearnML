
(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> let rec g a b f = if a == b then f (a) else f (a) * g (a+1) b f in
               g a b f