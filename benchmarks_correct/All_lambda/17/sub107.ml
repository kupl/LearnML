 type lambda = V of var | P of var * lambda | C of lambda * lambda
 and var = string

let rec checkWList ((ilist : string list),(met : lambda)) : bool = 
match (ilist,met) with
|(lst,P(id,m)) -> checkWList(id::lst, m)
|(lst,V id) -> List.mem id lst
|(lst,C(m1,m2)) -> checkWList(lst,m1) && checkWList(lst,m2)

let check (met : lambda) : bool = 
checkWList([],met)

