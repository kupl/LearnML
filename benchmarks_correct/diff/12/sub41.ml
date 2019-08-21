type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let rec diff (aexp,str) = 
	match aexp with
	|Const n -> Const 0
	|Var s -> if (s=str) then Const 1
		  else Const 0
	|Power (s,i) -> if (s=str) then Times[Const i; Power(s,i-1)]
			else Const 0
	|Times (hd::[]) -> diff(hd,str)
	|Times (hd::tl) -> Sum[Times[(diff (hd,str));Times tl];Times[hd;(diff (Times tl,str))]]
	|Sum (hd::[]) -> diff(hd,str)
	|Sum (hd::tl) -> Sum[(diff (hd,str));(diff (Sum tl,str))]
