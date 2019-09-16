type lambda = V of var
 | P of var * lambda
 | C of lambda * lambda
and var = string

let rec check : (string list)*lambda -> bool = fun (nlist,m) ->
 if nlist=[] then true
 else match m with
 | V(n) -> (List.exists (fun x -> n=x) nlist)
 | P(n,m1) -> check(n::nlist,m1)
 | C(m1,m2) -> check(nlist,m1) && check(nlist,m2)

let rec checklambda : lambda -> bool = fun m ->
 match m with
 | V(n) -> false
 | P(n,m1) -> check(n::[],m1)
 | C(m1,m2) -> checklambda(m1) && checklambda(m2)
