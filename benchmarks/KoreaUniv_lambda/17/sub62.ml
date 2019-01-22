(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec doesExist : (var * 'a list) -> bool
= fun (var, env) ->
  match env with 
  | [] -> false
  | hd::tl -> if(var = hd) then true else doesExist(var, tl)

let rec isBound : (lambda * 'a list) -> bool
= fun (lam, env) ->
  match lam with
  | V v -> if(doesExist (v, env)) then true else false
  | P (v, l) -> isBound(l, env@[v])
  | C (l1, l2) -> isBound(l1, env) && isBound(l2, env)

let rec check : lambda -> bool
= fun lam -> 
  match lam with
  | V v -> false
  | P (v, l) -> isBound(l, [v])
  | C (l1, l2) -> false


