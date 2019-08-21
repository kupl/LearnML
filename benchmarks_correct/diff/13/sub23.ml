type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

exception InvalidArgument

let rec diff (aexp, s) = 
	let rec diff_times s l r = 
		match r with
		| hd :: tl -> [Times (l @ [diff (hd, s)] @ tl)] @ (diff_times s (l @ [hd]) tl)
		| [] -> []
	in
	let rec diff_sub s aexp = 
		match aexp with
		| Const _ -> Const 0
		| Var x -> if x = s then Const 1 else Const 0
		| Power (x, n) -> if x <> s then Const 0 else if n = 0 then Const 0 else if n = 1 then Const 1 else Times [Const n; Power (x, n - 1)]
		| Sum l -> if l = [] then raise InvalidArgument else Sum (List.map (diff_sub s) l)
		| Times l -> if l = [] then raise InvalidArgument else Sum (diff_times s [] l)
	in diff_sub s aexp
