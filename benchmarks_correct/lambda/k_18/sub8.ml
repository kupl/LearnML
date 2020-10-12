type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string


type t = var list
let empty = []
let rec find bvar x = 
  match bvar with
    |[]-> false
    |hd::tl -> if hd = x then true else find tl x

let check : lambda -> bool
= fun lam -> 
  let rec my_check bvar lamb =
    match lamb with
    | V var -> find bvar var
    | P (var, lambda1) -> my_check (var::bvar) lambda1
    | C (lambda1, lambda2) -> (my_check bvar lambda1) && (my_check bvar lambda2)
  in my_check [] lam;;
  
check(
P ("a", P ("b", C (V "a", V "c")))
);;