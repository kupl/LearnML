(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type lenv = var list 

(*environment*)
let empty_lenv =[]
let extend_lenv v e = v::e
let rec apply_lenv e x =
 match e with
 |[] -> false 
 |v::tl -> if x = v then true  else apply_lenv tl x

let rec pcheck : lambda -> lenv -> bool
= fun lam e ->
match lam with 
|V v -> if (apply_lenv e v) then true else false
|P(v,l) -> let e1 =(extend_lenv v e)in  (pcheck l e1)            
|C(l1,l2) ->if (pcheck l1 e)&&(pcheck l2 e) then true else false 

let rec check : lambda -> bool
= fun lam ->
pcheck lam []
