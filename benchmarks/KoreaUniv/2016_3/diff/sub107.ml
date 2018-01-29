
 type aexp =
 | Const of int
 | Var of string
 | Power of string * int
 | Times of aexp list
 | Sum of aexp list


let rec diff : aexp * string -> aexp
 = fun (exp, var) ->
	match exp with
	| Const n -> Const 0
  	| Var x -> if x = var then Const 1 else Const 0
	| Power(s, n) ->
		if s = var then
		(match n with
		|0 -> Const 0
		|1 -> Const 1
		|2 -> Times [Const 2 ; Var var]
		|_ -> Times [Const n; Power(s, n-1)])
		else Const 0
	| Sum l ->
	(match l with
		[] -> Const 0
		| hd::[] -> (diff(hd,var))
		| hd::tl -> (Sum [diff(hd,var) ; diff(Sum tl, var)]))
	|Times l ->
	(match l with
		[] -> Const 1
		| hd :: [] -> (diff(hd, var))
		| hd :: tl ->
			(match hd with
				Const 1 -> (diff(Times tl, var))
				| Const n -> (Times[Const n ; diff(Times tl, var)])
				| _ -> (Sum [Times (diff(hd, var)::tl) ; Times [hd ; diff(Times tl, var)]]))) 
