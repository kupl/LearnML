(**********************)
(*   Problem 2        *)
(*********************)
type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
type env = var list

let extend_env x e = x::e
let rec apply_env e x =
  match e with
  | [] -> false
  | hd::tl -> if hd=x then true else apply_env tl x

let rec eval : lambda -> env -> bool
= fun l e ->
match l with
| V x -> if (apply_env e x)=true then true
         else false
| P (x,l1) -> eval l1 (extend_env x e)
| C (l1,l2) ->  (eval l1 e) && (eval l2 e)

let rec check : lambda -> bool
= fun lam -> (eval lam [])

