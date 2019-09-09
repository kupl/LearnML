type aexp = Const of int
		| Var of string
		| Power of string * int
		| Times of aexp list
		| Sum of aexp list

exception InvalidArgument

let rec diff (e, str) =
	match e with
	| Const _ -> Const 0
	| Var s ->
		if s = str then Const 1
		else Const 0
	| Power (s, n) ->
		if s = str then Times([Const n; Power(s, n-1)])
		else Const 0
	| Times aexplist ->
		if aexplist = [] then raise InvalidArgument
		else if List.length aexplist = 1 then diff (List.hd aexplist, str)
		else Sum (Times (diff (List.hd aexplist, str) :: List.tl aexplist) :: Times (List.hd aexplist :: diff (Times (List.tl aexplist), str) :: []) :: [])
	| Sum aexplist ->
		if aexplist = [] then raise InvalidArgument
		else if List.length aexplist = 1 then diff (List.hd aexplist, str)
		else Sum (diff (List.hd aexplist, str) :: diff (Sum (List.tl aexplist), str) :: [])
