(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec che lam l = match lam with
| V x -> let rec search lst = 
match lst with
| [] -> false
| hd::tl -> if hd = x then true else search tl
in search l
| P (x,ld) -> che ld (x::l)
| C (ld1, ld2) -> (che ld1 l) && (che ld2 l)
in che lam []
