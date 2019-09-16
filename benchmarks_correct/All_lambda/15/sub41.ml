type lambda = V of var
 | P of var * lambda
 | C of lambda * lambda
and var = string
(*
 * manually edited 
 * checkMetro -> check
 * check -> check_sub
 *)
let rec check_sub : (string list)*lambda -> bool = fun (nlist,m) ->
 if nlist=[] then true
 else match m with
 | V(n) -> (List.exists (fun x -> n=x) nlist)
 | P(n,m1) -> check_sub(n::nlist,m1)
 | C(m1,m2) -> check_sub(nlist,m1) && check_sub(nlist,m2)

let rec check : lambda -> bool = fun m ->
 match m with
 | V(n) -> false
 | P(n,m1) -> check_sub(n::[],m1)
 | C(m1,m2) -> check(m1) && check(m2)
