(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec complst : var list -> var -> bool
= fun lst v -> match lst with
|[] -> false
|hd::tl -> if (hd = v) then true else complst tl v

let rec checklst : var list -> lambda -> bool
= fun lst lam -> match lam with
| V x -> complst lst x
| P (x, l) -> checklst (x::lst) l
| C (l1, l2) -> checklst lst l1 && checklst lst l2

let rec check : lambda -> bool
= fun lam -> match lam with
| V x -> false
| P (x, l) -> checklst [x] l
| C (l1, l2) -> (check l1) && (check l2)