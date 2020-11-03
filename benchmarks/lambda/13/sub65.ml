type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string
(*
 * manually edited
 * checkMetro -> check 
 * check -> check_sub
 *)
let rec check met = 
	check_sub(met, [])

and check_sub (met,l) = 
	match met with
        |V n -> if(idSearch(n, l)) then true else false
        |P(n, metr) -> check_sub(metr, [n]@l)
        |C(metr1, metr2) -> check_sub(metr1, l) && check_sub(metr2, l)

and idSearch(id,l) =
	match l with
	|[] -> false
	|head::tail -> 	if(head = id) then true
		else idSearch(id,tail)
