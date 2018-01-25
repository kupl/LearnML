type formula =
		True 
	| False
	| Neg of formula
	| Or of formula * formula
	| And of formula * formula
	| Imply of formula * formula
	| Equiv of formula * formula;;

let rec eval : formula -> bool
= fun f -> true;;

let rec eval f =
		match f with
		True -> true |
		False -> false |
		Neg (f1) -> if f1 = True then false else true |
		Or (f1, f2) -> if (f1 = True && f2 = True) then false else true |
		And (f1, f2) -> if (f1 = True && f2 = True) then true else false |
		Imply (f1, f2) -> if (f1 = True && f2 = False) then false else true |
		Equiv (f1, f2) -> if f1 = f2 then true else false;;

