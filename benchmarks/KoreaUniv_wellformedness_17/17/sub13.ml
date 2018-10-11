(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec del x l = match l with
                |[] -> l
                |hd::tl -> if (x = hd) then del x tl else hd::(del x tl)

let rec proc lambda = match lambda with
                |V x -> [x]
                |P (x, la) -> del x (proc la)
                |C (la1, la2) -> (proc la1) @ (proc la2)

let check : lambda -> bool
= fun lam -> if (proc lam = []) then true else false
