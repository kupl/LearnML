(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec remove n ls 
= match ls with
|[]->[]
|hd::tl -> if hd=n then remove n tl else hd::(remove n tl)

let rec length ls
=match ls with
|[] ->0
|hd::tl -> 1+(length tl)

let rec union l1 l2 
=match l1 with
|[] -> l2
|hd::tl -> union tl (hd::l2)

let rec setfree lam
= match lam with
|V x -> [x]
|P(x, lam1) -> remove x (setfree lam1)
|C(lam1, lam2) -> union (setfree lam1) (setfree lam2)

let rec check : lambda -> bool
= fun lam -> 
let s = setfree lam in
(if length s = 0 then true else false)