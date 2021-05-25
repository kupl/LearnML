type aexp = Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let rec diff (exp, s) =
	match exp with
	| Const i -> Const 0
	| Var a -> if a = s then Const 1 else Const 0
	| Power (a, i) -> if a=s then begin if i=0 then Const 0
					    else if i=1 then Const 1
					    else Times[Const i; Power(a, i-1)]
				      end
			  else Const 0
	| Sum lst ->
		(match lst with
		| [] -> Const 0
		| hd::[] -> diff (hd, s)
		| hd::tl -> Sum [diff (hd, s); diff (Sum tl, s)])
	| Times lst ->
		match lst with
		| [] -> Const 0
		| _ -> Sum (tCheck (List.rev lst, (List.length lst)-1, s))

	and partT (lst, n, s) =
		match lst with
		| [] -> []
		| hd::tl -> if n = 0 then (diff (hd, s))::tl else hd::(partT (tl, (n-1),s ))
	and tCheck (lst, n, s) =
		match lst with
		| [] -> []
		| _ ->  if n = 0 then (Times (partT (lst , 0, s)))::[]
			else ((Times (partT (lst, n, s)))::(tCheck(lst, n-1,s)))
			
(*			match hd with
			| Const x -> diff(tl, s)
			| VAL x -> diff(hd, s)::diff(tl, s)
			| Power x -> diff(hd, s)::diff(tl, s)
			| Times x -> diff(hd, s)@diff(tl, s)
			| Sum x -> diff(hd, s)@diff(tl, s)
*)
