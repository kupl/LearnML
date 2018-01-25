type formula =
	True
	|False
	|Neg of formula
	|Or of formula * formula
	|And of formula * formula
	|Imply of formula * formula
	|Equiv of formula * formula;;

let rec eval formula =
	match formula with
		True -> true
		|False -> false
		|Neg formula -> if eval formula = true then false else 
			true
		|Or (formula1 , formula2) -> if 
			eval formula1 = true then true else if 
			eval formula2 = true then true else false
		|And (formula1 , formula2) -> if 
			eval formula1 = false then false else if 
			eval formula2 = false then false else true
		|Imply (formula1 , formula2) -> if
			eval formula1 = true then if eval formula2 = false then false	else
			true else true
		|Equiv (formula1 , formula2) -> if
			eval formula1 = true then if eval formula2 = true then true else false else if
			eval formula2 = false then true else false;;
