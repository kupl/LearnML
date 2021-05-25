type aexp = 
	  Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let rec diff (exp, var) =
	match exp with
	| Const(_) -> Const 0
	| Var(str) -> if str = var then Const 1 else Const 0
	| Power(str, n) -> if str = var then Times( (Const n) :: (Power(str, n-1)) :: []) else Const 0
	| Times([]) | Sum([]) -> Const 0
	| Times(hd::tl) -> Sum(
						(Times( (diff(hd, var))::tl )) :: 
						(Times( hd :: diff(Times(tl), var) :: [])) :: [])
	| Sum(hd::tl) -> Sum( (diff(hd, var)) :: (diff(Sum(tl), var)) :: [])
	
