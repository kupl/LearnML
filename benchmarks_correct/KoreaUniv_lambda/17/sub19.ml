
 (**********************) 
 (* Problem 2 *) 
 (**********************) 
type lambda = 
 V of var 
 | P of var * lambda 
 | C of lambda * lambda 
 and var = string 
(* TODO *) 
let rec vcheck : lambda * (var list) -> bool
 = fun (lam, clist)  ->
 match lam with
 |V var -> (match clist with
 	|hd :: tl -> if var = hd then true else vcheck(V var, tl)
 	|[] -> false)
 |P (var, lam1) -> vcheck(lam1, var::clist)
 |C (lam1, lam2) -> vcheck(lam1, clist) && vcheck(lam2, clist)


let rec check : lambda -> bool 
 = fun lam -> vcheck(lam, []);;

