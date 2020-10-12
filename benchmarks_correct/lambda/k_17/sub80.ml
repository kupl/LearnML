(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam ->  (* TODO *)
  let rec help lam env =
    match lam with
    | V a -> List.mem a env
    | P (a, b) -> help b (a::env)
    | C (a, b) -> (help a env && help b env)
      in help lam []
