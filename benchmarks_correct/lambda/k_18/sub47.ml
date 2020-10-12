type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

type tenv = var -> bool
let empty_tenv = fun y -> false
let extend x tenv = fun y -> if x = y then true else (tenv y)

let rec check_aux : tenv -> lambda -> bool
= fun tenv lam ->
  match lam with
  | V a -> tenv a  
  | P (a,lam) -> check_aux (extend a tenv) lam
  | C (lam1,lam2) -> (check_aux tenv lam1) && (check_aux tenv lam2)

let check : lambda -> bool
= fun lam -> check_aux empty_tenv lam

