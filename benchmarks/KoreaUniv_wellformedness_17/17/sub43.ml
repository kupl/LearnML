(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type env2 = var list

let rec findVar : var -> env2 -> bool
= fun x env2 ->
  match env2 with
  | hd::tl -> if hd=x then true else findVar x tl
  | [] -> false
  

let rec test : lambda -> env2 -> bool
= fun lam env2 ->
  match lam with
  | V x -> findVar x env2
  | P (x,l) -> let newEnv2=(x::env2) in test l newEnv2
  | C (l1,l2) -> (test l1 env2)&&(test l2 env2)

let rec check : lambda -> bool
= fun lam -> (* TODO *)
  test lam []