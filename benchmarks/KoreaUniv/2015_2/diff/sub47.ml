type aexp = 
	| Const of int 
	| Var of string 
	| Power of string * int 
	| Times of aexp list 
	| Sum of aexp list 
let rec diff (aexp, string) = 
	match aexp with 
	Const n -> Const 0
	|Var x -> 
		if x=string then Const 1 
		else aexp
	|Power (x, n) -> 
		if x=string then Times [Const n; Power (x, n-1)] 
		else aexp
	|Times l -> 
		(
			match l with 
			[] -> Const 0
			|hd::tl -> Sum([Times(diff(hd, string)::tl)]@[Times(hd::[diff(Times(tl), string)])])
		)
	|Sum l ->
		(
			match l with 
			[] -> Const 0
			|hd::tl -> Sum([diff (hd, string)]@[diff (Sum (tl), string)])
		);; 
		