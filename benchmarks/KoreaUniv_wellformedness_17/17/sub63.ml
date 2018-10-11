(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec find x env =
match env with
|[] -> false
|hd::tl -> if hd = x then true else find x tl in
let rec del x env =
match env with
|[] -> []
|v::tl -> if v = x then tl else v::(del x tl) in
let rec free l env =
match l with
|V v -> if (find v env) then env else v::env
|P (v, l1) -> del v (free l1 env)
|C (l1, l2) -> let env1 = free l1 env in free l2 env1
in let env0 = [] in if (free lam env0) = [] then true else false

