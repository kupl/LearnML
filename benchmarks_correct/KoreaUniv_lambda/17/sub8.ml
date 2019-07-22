(**********************)
(*   Problem 2        *)
(**********************)

(* environment *)
let empty_lam = []
let extend_lam x e = x::e
let rec apply_lam e x = 
  match e with
  | [] -> false
  | y::tl -> if x = y then true else (apply_lam tl x)


type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec lamb lam env = 
 match lam with 
  | V(va) -> apply_lam env lam
  | P(va,la1) -> let env' = extend_lam (V(va)) env in lamb la1 env'
  | C(la1,la2) -> if (lamb la1 env) && (lamb la2 env) then true else false
in (lamb lam empty_lam)