type aexp = 
	| Const of int 
	| Var of string 
	| Power of string * int 
	| Times of aexp list 
	| Sum of aexp list 
let rec diff (aexp, str) = 
	match aexp with 
	Const n -> Const 0
	|Var x -> 
		if x=str then Const 1 
		else aexp
	|Power (x, n) -> 
		if x=str then Times [Const n; Power (x, n-1)] 
		else aexp
	|Times l -> 
		(
			match l with 
			[] -> Const 0
			|hd::tl -> Sum([Times(diff(hd, str)::tl)]@[Times(hd::[diff(Times(tl), str)])])
		)
	|Sum l ->
		(
			match l with 
			[] -> Const 0
			|hd::tl -> Sum([diff (hd, str)]@[diff (Sum (tl), str)])
		);; 
		
