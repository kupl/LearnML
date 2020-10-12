(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec fcheck: var -> var list -> bool
=fun v fl -> match fl with
 [] ->false
|hd::tl ->(hd=v)||(fcheck v tl);;

let rec check2 : lambda -> var list -> bool
=fun lam fl -> match lam with
 V(v) -> fcheck v fl
|P(v,l) -> check2 l ([v]@fl)
|C(l1,l2) -> (check2 l1 fl)&&(check2 l2 fl);;


let rec check : lambda -> bool
= fun lam ->
check2 lam [];;
