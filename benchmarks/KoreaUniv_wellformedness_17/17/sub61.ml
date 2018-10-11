(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
  let rec vcheck: var -> var list -> bool
  =fun v lst -> 
  (match lst with
    | [] -> false
    | hd::tl -> if (hd=v) then true else (vcheck v tl)
  ) in
    let rec vlist: lambda -> var list -> bool
    = fun lam lst ->
    (match lam with
      | V (v) -> (vcheck v lst)
      | P (v,l) -> (vlist l (v::lst))
      | C (l1, l2) -> (vlist l1 lst) && (vlist l2 lst)
    ) in
      vlist lam []

