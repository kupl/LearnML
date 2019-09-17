type aexp = Const of int
				| Var of string
				| Power of string * int
				| Times of aexp list
				| Sum of aexp list

exception InvalidArgument

let rec diff (aexp, v) =
	let rec aux (aexp, v) =
		match aexp with
		| Const c -> Const 0
		| Var x -> if x = v then Const 1 else Const 0
		| Power (x, n) -> if x = v
											then Times[ (Const n); (Power (x, n - 1)) ]
											else Const 0
		| Times [] -> Const 0
		| Times ((Const 0)::tl) -> Const 0
		| Times (hd::tl) -> Sum (Times (aux (hd, v)::tl)::(Times (hd::aux ((Times tl),v)::[]))::[])
		| Sum aexplist -> Sum (List.map (fun aexp -> aux (aexp, v)) aexplist) in 
	match aexp with
	| Times [] -> raise InvalidArgument
	| Sum [] -> raise InvalidArgument
	| _ -> aux (aexp, v)
