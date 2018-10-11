(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let check : lambda -> bool
= fun lam ->
  let rec check' = fun lam' vars ->
    match lam' with
    | V x -> List.exists (fun a -> a = x) vars
    | P (x, l) -> check' l (x :: vars)
    | C (l1, l2) ->
      (check' l1 vars) && (check' l2 vars)
  in
  check' lam []
