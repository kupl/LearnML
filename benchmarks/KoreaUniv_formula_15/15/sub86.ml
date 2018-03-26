type formula =
	True
	| False
	| Neg of formula
	| Or of formula * formula
	| And of formula * formula
	| Imply of formula * formula
	| Equiv of formula * formula 

let rec eval formula = 
	match formula with 
	True -> True
	|False -> False
	|Neg True -> False
	|Neg _ -> True
	|Or (False, False) -> False
	|Or (_, _) -> True
	|And(True, True) -> True
	|And (_, _) -> False
	|Imply (True, False) -> False
	|Imply (_, _) -> True
	|Equiv (True, True) -> True
	|Equiv (False, False) -> True
	|Equiv (_, _) -> False 
