(* problem 3*)
type formula =
 |True
 |False
 |Var of string
 |Neg of formula
 |And of formula * formula
 |Or of formula * formula
 |Imply of formula * formula
 |Iff of formula * formula

let rec sat : formula -> bool
=fun f -> match f with
|True -> true
|False -> false
|Var v -> if v = "P" then true else false
|Neg n -> if (sat n) == true then false else true
|And (a, b) -> if ((sat a) == true) && ((sat b) == true) then true else false
|Or (a, b) -> if((sat a) == true) || ((sat b) == true) then true else false
|Imply (a, b) -> if (sat a) = true then true 
				else if (sat b) == true then true else false
|Iff (a, b) -> if ((sat a)==true) && ((sat b)==true) then true
				else if ((sat b)==false) && ((sat b)==false) then true
				else false;;
