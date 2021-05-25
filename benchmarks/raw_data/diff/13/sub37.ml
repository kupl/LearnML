type aexp = Const of int
		| Var of string
		| Power of string * int
		| Times of aexp list
		| Sum of aexp list
exception InvalidArgument

let rec diff (aexp, str) =
		match aexp with
		| Const i -> Const 0
		| Var s ->
			if s = str then Const 1
			else Const 0
		| Power (s, i) ->
			if s = str then Times ([Const i] @ [Power (s, i-1)])
			else Const 0
		| Times (hd::tl) -> 
			if tl = [] then diff (hd, str)
			else Sum ([Times ([diff (hd, str)] @ tl)] @ [Times ([hd] @ [diff (Times tl, str)])])
		| Times [] -> raise InvalidArgument
		| Sum (hd::tl) ->
			if tl = [] then diff (hd, str)
			else Sum ([diff (hd, str)] @ [diff (Sum tl, str)])
		| Sum [] -> raise InvalidArgument
