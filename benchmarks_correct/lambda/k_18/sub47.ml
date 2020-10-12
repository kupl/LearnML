type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec check_aux : var list -> lambda -> bool
= fun tenv lam ->
  match lam with
  | V a -> List.mem a tenv
  | P (a,lam) -> check_aux (a::tenv) lam
  | C (lam1,lam2) -> (check_aux tenv lam1) && (check_aux tenv lam2)

let check : lambda -> bool
= fun lam -> check_aux [] lam

