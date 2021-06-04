(**********************)
(*   Problem 2        *)
(**********************)

(* environment *)

let empty_env' = []

let extend_env' x e = x :: e

let rec apply_env' e x =
  match e with
  | [] -> false
  | y :: tl -> if x = y then true else apply_env' tl x


type lambda = V of var | P of var * lambda | C of lambda * lambda

and var = string

let rec check : lambda -> bool =
 fun lam ->
  let rec check' lam env =
    match lam with
    | V x -> apply_env' env lam
    | P (x, f) ->
        let proc = extend_env' (V x) env in
        check' f proc
    | C (f1, f2) -> if check' f1 env = check' f2 env then true else false
  in
  check' lam empty_env'
