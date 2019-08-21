type aexp = Const of int | Var of string | Power of string*int | Times of aexp list | Sum of aexp list

let rec simplify a =
	match a with
	| Power(_, 0) -> Const 1
	| Times l ->
		if List.mem (Const 0) l then Const 0
		else if List.length l = 1 then simplify(List.nth l 1)
		else Times l
	| Sum ll -> 
		Sum (List.map simplify ll)
	| _ -> a

let rec diff(a, str) =
	match a with
	| Const n -> Const 0
	| Var s ->
		if s = str then Const 1
		else Const 0
	| Power(s, p) ->
		if p = 0 then Const 0
		else if s = str then
			if p = 1 then Const 1
			else simplify(Times[Const p; Power(s, p-1)])
		else Const 0
	| Times l ->
		(match l with
		| [] -> invalid_arg "invalid value!"
		| h::[] -> simplify(diff(h, str))
		| h::t -> simplify(Sum[Times[diff(h, str); Times t]; Times[h; diff(Times t, str)]]))
	| Sum ll ->
		(match ll with
		| [] -> Const 0
		| h::[] -> simplify(diff(h,str))
		| h::t -> simplify(Sum (List.map (fun aa -> diff(aa, str)) ll)))

