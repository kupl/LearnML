type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

exception InvalidArgument

let rec diff (a, s) =
match a with
| Const c -> Const 0
| Var c -> if c=s then Const 1
		else Const 0
| Power (c, n) -> if c=s then 
			if n!=0 then Times (Const n :: [Power (c, n-1)])
			else Const 0
		else Const 0
| Times [] -> raise InvalidArgument
| Times (hd::[]) -> diff (hd, s)
| Times (hd::tl) -> Sum [Times [ (diff (hd, s)) ; Times tl] ; (Times [ hd ; (diff (Times tl, s))])] 
| Sum [] -> raise InvalidArgument
| Sum (hd::[]) -> diff (hd, s)
| Sum (hd::tl) -> Sum [diff (hd, s) ; diff(Sum tl, s)]
