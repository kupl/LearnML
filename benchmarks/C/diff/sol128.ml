
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> 
  	match exp with
	| Const _ -> Const 0
	| Var str -> if (var = str) then Const 1 else Const 0
	| Power (str,num) -> 
		if (str=var) then
			begin
			match num with
			| 0 -> Const 0
			| _ -> Times [Const num; Power (str, num-1)]
			end
		else Const 0
	| Times [] -> Const 0
	| Times (h::t) -> Sum [Times[h;diff(Times t,var)(*This is aexp!!*)];Times (diff(h,var)::t)] (* VERY hard!!*)
	| Sum exp -> Sum ( List.map(fun x -> diff(x,var)) exp)