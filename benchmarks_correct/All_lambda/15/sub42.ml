type lambda = V of var
 | P of var * lambda
 | C of lambda * lambda
and var = string

let rec check2 : (string list)*lambda -> bool = fun (nlist,m) ->
 if nlist=[] then true
 else match m with
 | V(n) -> (List.exists (fun x -> n=x) nlist)
 | P(n,m1) -> check2(n::nlist,m1)
 | C(m1,m2) -> check2(nlist,m1) && check2(nlist,m2)

let rec check : lambda -> bool = fun m ->
 match m with
 | V(n) -> false
 | P(n,m1) -> check2(n::[],m1)
 | C(m1,m2) -> check(m1) && check(m2)
