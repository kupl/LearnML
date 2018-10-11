(* Problem 2 *) (**********************) 
type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec helper : var -> var list -> bool
= fun v vlist -> match vlist with
| [] -> false
| hd::tl -> if (v=hd) then true else helper v tl

let rec checker : lambda -> var list -> bool -> bool
= fun l lst b -> match l with
| V x -> helper x lst
| P (v1, l1) -> (checker l1 (v1::lst) b) && b
| C (l1, l2) -> ((checker l1 lst b) && (checker l2 lst b)) && b

let rec check : lambda -> bool
= fun l -> checker l [] true