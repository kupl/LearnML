type aexp = Const of int
	|Var of string
	|Power of string * int
	|Times of aexp list
	|Sum of aexp list

let rec diff (exp ,s) = 
	let rec mul (a, b) = 
		match b with
		| [] -> []
		| hd::tl ->  (Times ( (diff (hd,s))::(List.append a tl)) ) ::mul(hd::a, tl)
	in
	let rec hap a = 
		match a with
		| [] -> []
		| hd::tl -> (diff (hd, s))::(hap tl)
	in
	match exp with
	| Const a -> Const 0
	| Var a -> if a = s then Const 1 else Const 0
	| Power (a, b) -> if a = s then Times [Const b; Power (a, b-1)] else Const 0
	| Times a -> Sum (mul ([], a))
	| Sum a -> Sum (hap a)
