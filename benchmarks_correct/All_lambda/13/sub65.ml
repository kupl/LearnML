type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec check met = 
	check(met, [])

and check (met,l) = 
	match met with
        |V n -> if(idSearch(n, l)) then true else false
        |P(n, metr) -> check(metr, [n]@l)
        |C(metr1, metr2) -> check(metr1, l) && check(metr2, l)

and idSearch(id,l) =
	match l with
	|[] -> false
	|head::tail -> 	if(head = id) then true
		else idSearch(id,tail)
