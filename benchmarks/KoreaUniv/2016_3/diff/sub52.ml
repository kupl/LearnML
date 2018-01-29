
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
		match exp with
		Const(n) -> Const 0
	| Var(v) -> if var = v then Const 1 else Const 0
	|	Power(v,n) ->
			if n > 1 && var = v then
				Times[Const n; Power(v,(n-1))]
			else if n == 1 && var = v then
				Const 1
			else
				Const 0

	| Times(lst)->
		let rec diffTimes : aexp list * string * aexp list * aexp list -> aexp
		= fun (lst, var,sumlst,prelst)->		
			match lst with
				[]->Sum(sumlst)
			|	n::lst2 ->
					let a = Times(prelst @ diff(n,var) :: lst2) in
					diffTimes(lst2, var, sumlst @ a::[], prelst@n::[])
			in
		diffTimes (lst,var,[],[])

	|	Sum(lst)->
		let rec diffSum : aexp list * string * aexp list -> aexp
		= fun (lst,var,sumlst) ->
			match lst with
				[] -> Sum(sumlst)
			|	n::lst2 -> 
					diffSum(lst2,var,sumlst @ diff(n,var)::[])
			in
		diffSum(lst,var,[])
