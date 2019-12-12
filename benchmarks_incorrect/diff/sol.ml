type aexp =
	| Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let rec map : (aexp * string -> aexp) -> aexp list * string  -> aexp list
= fun f (l, x) ->
	match l with
	| [] -> []
	| hd::tl -> (f (hd, x))::(map f (tl, x))

let rec diff : aexp * string -> aexp
= fun (e, x) ->
	match e with
	| Const n -> Const 0
	| Var a -> if (a <> x) then Const 0 else Const 1
	| Power (a, n) -> 
		if n <0 then raise (Failure "Invalid") 
		else if	(n = 0) || (a <> x) then Const 0
		else Times [Const n; Power (a, n-1)]
	| Times l ->
		begin 
			match l with
			| [] -> raise (Failure "Invalid")
			| [hd] -> diff (hd, x)
			| hd::tl -> Sum [Times ((diff (hd, x))::tl); Times [hd; diff (Times tl, x)]]
		end
	| Sum l -> 
		begin match l with
		| [] -> raise (Failure "Invalid")
		| _ -> Sum (map diff (l, x))
		end