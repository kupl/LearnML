(* problem 3*)
type formula =
	True
 |False
 |Var of string
 |Neg of formula
 |And of formula * formula
 |Or of formula * formula
 |Imply of formula * formula
 |Iff of formula * formula

let rec satstr : formula -> formula
= fun g -> match g with
	True -> True
 |False -> False
 |Var s -> Var s
 |Neg (Neg (g1)) -> satstr g1
 |Neg (g1) -> if (satstr g1)=True then False else if (satstr g1)=False then True else Neg (satstr g1)
 |And (g1,g2) ->
		if (satstr g1)=(satstr (Neg (g2))) then False
		else if (satstr g1)=True && (satstr g2)=True then True
		else if (satstr g1)=False && (satstr g2)=False then False
		else True
 |Or (g1,g2) -> if (satstr g1)=False && (satstr g2)=False then False else True
 |Imply (g1, g2) -> if (satstr g1)=True && (satstr g2)=False then False else True
 |Iff (g1, g2) -> if (satstr g1)=(satstr g2) then True else False;;
		
let rec sat : formula -> bool
= fun f -> match f with
	True -> true
 |False -> false
 |Var (s) -> true
 |_ -> sat (satstr f);;

