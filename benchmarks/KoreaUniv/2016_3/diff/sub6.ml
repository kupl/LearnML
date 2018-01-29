
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
	
	let rec diffRec : aexp list * string -> aexp list =
	fun (aexpList, var) ->
		match aexpList with
			| [] -> []
			| hd :: tl -> diff(hd, var) :: diffRec (tl, var)
  and diff : aexp * string -> aexp
  = fun (exp, var) ->
		match exp with
			| Sum(aexpList) -> Sum(diffRec (aexpList, var))
			| Power(string1, int1) -> if(string1 = var) then( 
																	if int1 = 0 then Const 0
																	else if int1 = 1 then Const 1
																	else Times[Const int1;Power(var, int1-1)]
)
																else Const 0
			| Times(aexpList) -> (match aexpList with
														| [] -> Sum[]
														| hd :: tl ->
																	if tl = [] then diff (hd, var) else
																	 Sum[Times (diff (hd, var)::tl) ; Times[hd; diff(Times (tl), var)]])
			| Var(string1) -> diff(Power(string1, 1), var)
			| Const(int1) -> Const 0 						