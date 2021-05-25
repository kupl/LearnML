type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

exception InvalidArgument

let rec diff: aexp * string -> aexp
	= fun (alexp, dv) ->
		match alexp with
		| Const i -> Const 0
		| Var str -> if (str = dv) then Const 1 else Const 0
		| Power (str, i) -> if (str = dv) then Times(Const i::Power(str, i - 1)::[]) else Const 0
		| Times [] | Sum [] -> raise InvalidArgument
		| Times (hd::[]) -> diff(hd, dv)
		| Times (hd::tl) -> Sum (Times (diff(hd, dv)::tl)::Times (hd::diff(Times tl, dv)::[])::[])
		| Sum alexpList -> Sum(List.map (fun alexp -> diff (alexp, dv)) alexpList)
