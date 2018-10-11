(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
and env = var list

(* environment *)
let empty_env = []
let extend_env x e = x::e
let rec apply_env e x = 
  match e with
  | [] -> false
  | hd::tl -> if x = hd then true else apply_env tl x

let rec check : lambda -> bool
= fun lam -> let rec func lam env = (match lam with | V(a) -> if (apply_env env a) = true then true else false
					   	    | P(a, l) -> (func l (extend_env a env))
					 	    | C(l1, l2) -> ((func l1 env) && (func l2 env))) in func lam empty_env 

















