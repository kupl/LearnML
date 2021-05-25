(*2*)
type lambda = V of var
             |P of var * lambda
             |C of lambda * lambda
             and var = string

 let rec check : lambda -> bool
 = fun lam ->
  let rec sub : lambda -> var list -> bool
   = fun lam v1 ->
    (match lam with
      | V v -> ( (List.exists (fun x->x =v) v1))
      | P (v,lam) -> ( sub lam (v :: v1))
      | C (lam1, lam2) -> if (sub lam1 v1) then (sub lam2 v1) else false) in sub lam []
