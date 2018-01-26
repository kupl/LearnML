(*problem3*)

type formula =
	True
	|False
	|Var of string
	|Neg of formula
	|And of formula * formula
	|Or of formula * formula
	|Imply of formula * formula
	|Iff of formula * formula


	

let rec sat : formula -> bool
= fun f ->
match f with
|True -> true
|False -> false
|Var a -> true
|Neg form -> if sat form = true then false else true
|And (f1,f2) -> if (sat f1 && sat f2) || (sat f1 && (sat (Neg f2))||((sat(Neg f1)) && sat f2)||((sat (Neg f1)) && (sat (Neg f2)) )) = true then true else false
|Or (f1, f2) -> if (sat f1 || sat f2) ||(sat f1 || (sat ( Neg f2)) || ((sat(Neg f1)) || sat f2)||((sat(Neg f1)) || (sat (Neg f2)) )) = true then true else false
|Imply(f1, f2) ->if sat f2 = true then sat f1=true else sat f1 = false
|Iff(f1,f2) -> if ((sat f1 = true && sat f2 = true) && (sat f1 = false && sat f2= false))||(( sat (Neg f1) = true && sat f2 = true) && (sat (Neg f1) = false && sat f2= false)) ||((sat f1 = true && sat (Neg f2) = true) && (sat f1 = false && sat (Neg f2)= false)) then true else false
