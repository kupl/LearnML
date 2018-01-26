(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list


  let rec processtimelist : aexp * string -> aexp
  = fun (l, var) ->
  	match l with
		| Const i ->
			Const i
		| Var str->
			if str=var then
				Const 0
			else
				Var str
		| Power (str, i) ->
			if str=var then
	  			(match i with
	  				|2 -> Times [Const 2; Var str]
	  				|1 -> Const 1
	  				|0 -> Const 0
	  				|_ -> Times [Const i; Power (str, i-1)]
	  			)
			else
				Power (str, i)
		| Times (hd::tl) ->
  			if findlist (hd::tl, var) > 0 then
				Sum ([Times ((processtimelist (hd,var))::tl)] @[Times ([hd]@[processtimelist (Times tl, var)])])
  			else
  				Const 1
		|_ ->
			makediff (l, var)
  					
  					
  and processsumlist : aexp list * string -> aexp list
  = fun (l, var) ->
  	match l with
  		| [] -> []
  		| hd::tl ->
  			match hd with
  				| Var str->
  					if str=var then
  						Const 1::(processsumlist (tl, var))
  					else
  						Const 0::(processsumlist (tl, var))
  				|_ ->
  					makediff (hd, var)::(processsumlist (tl, var))

  and findlist : aexp list * string -> int
  = fun (l, str) ->
	match l with
		| [] -> 0
		| hd::tl ->
			match hd with
				| Const i -> 
					findlist (tl, str)
				| Var str2 ->
					if str=str2 then
						1 + findlist (tl, str)
					else
						findlist (tl, str)
  				| Power (str2, i) -> 
					if str=str2 then
						i + findlist (tl, str)
					else
						findlist (tl, str)
				| Times l ->
					findlist(l, str) + findlist (tl, str)
				| Sum l ->
					findlist(l, str) + findlist (tl, str)
						
		
  and makediff : aexp * string -> aexp
  = fun (exp, var) ->
	  match exp with
  		| Const i ->
  			Const 0
  		| Var str ->
			if str=var then
				Const 1
			else
				Const 0
  		| Power (str, i) -> 
  			if str=var then
	  			(match i with
	  				|2 -> Times [Const 2; Var str]
	  				|1 -> Const 1
	  				|0 -> Const 0
	  				|_ -> Times [Const i; Power (str, i-1)]
	  			)
	  		else
	  			Const 0
		| Times (hd::tl) ->
  			if findlist (hd::tl, var) > 0 then
				Sum ([Times ((processtimelist (hd,var))::tl)] @[Times ([hd]@[processtimelist (Times tl,var)])])
  			else
  				Const 1
  		| Times [] ->
  			Const 0
  		| Sum (hd::tl) ->
  			if findlist (hd::tl, var) > 0 then
  				Sum ([makediff(hd, var)]@[makediff(Sum tl, var)])
  			else
  				Const 1
  		| Sum [] ->
  			Const 1

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
  	makediff (exp, var)
  			
end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let rec branch_weight : branch -> int
  = fun br ->
  	match br with
  		| SimpleBranch (len, wei) ->
  			wei
  		| CompoundBranch (len, (br1 , br2)) ->
			(branch_weight br1) + (branch_weight br2)
			
  let mobile_weight : mobile -> int
  = fun mob ->
  	match mob with
  		| br1, br2 ->
  			(branch_weight br1) + (branch_weight br2)

  let rec balanced : mobile -> bool
  = fun mob ->
  	match mob with
  		| CompoundBranch(length1, mobile1), CompoundBranch(length2, mobile2) ->
				(balanced mobile1) && (balanced mobile2) && (((mobile_weight mobile1)*length1) = ((mobile_weight mobile2)*length2))
  		| CompoundBranch(length1, mobile1), SimpleBranch(length2, weight2) ->
				(balanced mobile1) && (((mobile_weight mobile1)*length1) = (weight2*length2))
  		| SimpleBranch(length1, weight1), CompoundBranch(length2, mobile2) ->
				(balanced mobile2) && ((weight1*length1) = ((mobile_weight mobile2)*length2))
  		| SimpleBranch(length1, weight1), SimpleBranch(length2, weight2) ->
				((weight1*length1) = (weight2*length2))
				
end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calx : exp -> int -> int
  = fun exp int ->
  	match exp with
  		| X ->
  			int
  		| INT (a) ->
  			a
  		| ADD (a, b) ->
  			(calx a int) + (calx b int)
  		| SUB (a, b) ->
  			(calx a int) - (calx b int)
  		| MUL (a, b) ->
  			(calx a int) * (calx b int)
  		| DIV (a, b) ->
  			(calx a int) / (calx b int)
  		| SIGMA (a, b, c) ->
		  	if (calx a 0) != (calx b 0) then
		  		if (calx a 0) < (calx b 0) then
  					(calx (SIGMA(INT((calx a 0) + 1), b, c)) (calx a int)) + (calx c (calx a int))
  				else
  					(calx (SIGMA(INT((calx a 0) - 1), b, c)) (calx a int)) + (calx c (calx a int))
  			else
				calx c (calx a int)
  		|_ ->
			int
  		
  let calculator : exp -> int
  = fun exp ->
  	calx exp 0
  	
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec findvariable : var -> var list -> bool
  = fun v l ->
  	match l with
  		| [] -> false
  		| hd::tl -> if hd = v then true else findvariable v tl

  let rec checkcheck : exp -> var list -> bool
  = fun exp l ->
  	match exp with
  		| V a ->
  			findvariable a l
  		| P (a, b) ->
  			checkcheck b (l@[a])
  		| C (a, b) ->
  			(checkcheck a l)&&(checkcheck b l)

  let check : exp -> bool
  = fun exp ->
  	checkcheck exp []
end
