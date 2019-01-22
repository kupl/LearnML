
type aexp = 
	| Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list


let rec diff : aexp*string -> aexp
	= fun (ae,va) -> match ae with
	| Const a -> Const 0
	| Var x -> if x = va then Const 1 else Const 0
	| Power (x,a) -> if x = va then Times[Const a; Power(x, a-1)] else Const 0
	| Times [] -> Const 0
	| Times (hd::[]) -> diff(hd,va)
	| Times (hd::tl) -> Sum[Times(diff(hd,va)::tl); Times[hd; diff(Times tl,va)]]
	| Sum [] -> Const 0
	|	Sum (hd::[]) -> diff(hd,va)
	| Sum (hd::tl) -> Sum[diff(hd,va); diff(Sum tl, va)] 
