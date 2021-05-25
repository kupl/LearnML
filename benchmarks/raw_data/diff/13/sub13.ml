type aexp = Const of int
		| Var of string
		| Power of string * int
		| Times of aexp list
		| Sum of aexp list

exception InvalidArgument

let rec diff (aexp, str) =
	match aexp with
	| Const x -> Const 0
	| Var x -> 
		if x=str then Const 1
		else Const 0
	| Power (s, i) ->
		if s=str then (
			if i=1 then Const 1
			else if i=0 then Const 0
			else Times ([Const i; Power (s, i-1)]))
		else Const 0
	| Times l -> 
		if l=[] then raise InvalidArgument
		else if (check (l, str))=true then Sum (gettimes (l, str))
		else Const 0
	| Sum l -> 
		if l=[] then raise InvalidArgument
		else Sum (getsum (l, str))

and check (l, str) = 
	match l with
	| hd::tl -> (
		match hd with
		| Var x ->
			if x=str then true
			else check (tl, str)
		| Power (x, i) -> 
			if x=str then true
			else check (tl, str)
		| Times x | Sum x -> 
			if check (x, str)=false then check (tl, str)
			else true
		| _ -> check (tl, str))
	| [] -> false

and gettimes (l, str) =
	match l with
	| hd::tl ->
		if tl=[] then [diff (hd, str)]
		else [Times ([diff (hd, str)] @ tl)] @ [Times ([hd] @ [Sum (gettimes (tl, str))])]
	| [] -> []

and getsum (l, str) = 
	match l with
	| hd::tl -> [diff (hd, str)] @ getsum (tl, str)
	| [] -> []
	
