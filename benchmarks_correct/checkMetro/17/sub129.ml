type lambda = V of var
|P of var * lambda
|C of lambda * lambda
and var = string

let rec check : lambda -> bool = fun met ->
match met with
|V n -> false
|P (var , met) -> checkArea(met,var::[])
|C (m1,m2) -> check m1 && check m2
and checkArea : lambda * string list -> bool = fun (m,nl) ->
match (m,nl) with
|(V var,[]) -> false 
|(V var,n::nl) -> if n=var then true else checkArea(V var,nl)
|(C(m1,m2),nl) -> checkArea(m1,nl) && checkArea(m2,nl)
|(P(var,met),nl) -> checkArea(met,var::nl)
