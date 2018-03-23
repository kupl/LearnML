type aexp = | Const of int | Var of string | Power of string * int | Times of aexp list | Sum of aexp list 
let rec diff a b =
	match a with
	Const a -> Const 0
	| Var l -> if l = b then Const 1
	else Const 0
	| Power (a1,b1) -> if b = a1 then
		if b1 = 2 then
		Times [Const 2; Var a1]
		else
		Times [Const b1; Power(a1,b1-1)]
	else
		Const 0
	| Times [hd;tl] -> Times[hd;(diff tl b)]
	| Sum p -> (match p with
	[x] -> (diff x b)
	| hd :: tl -> Sum [(diff hd b);(diff (Sum tl) b) ])
	
