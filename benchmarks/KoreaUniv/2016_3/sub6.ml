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

	let rec mobileWeight : mobile -> weight
	=fun mobile ->
		match mobile with
			| (SimpleBranch(length1, weight1), SimpleBranch(length2, weight2))
				-> weight1 + weight2
			| (CompoundBranch(length1, mobile1), SimpleBranch(length2, weight2))
				-> mobileWeight mobile1 + weight2
			| (SimpleBranch(length1, weight1), CompoundBranch(length2, mobile2))
				-> weight1 + mobileWeight mobile2
			| (CompoundBranch(length1, mobile1), CompoundBranch(length2, mobile2))
				-> mobileWeight mobile1 + mobileWeight mobile2

  let rec balanced : mobile -> bool
  = fun mob -> 
		match mob with
			| (CompoundBranch(length1, mobile1), CompoundBranch(length2, mobile2))
				-> if balanced(mobile1) && balanced(mobile2) then
						if length1 * mobileWeight(mobile1) = length2 * mobileWeight(mobile2) then true
						else false
					else false
			| (SimpleBranch(length1, weight1), CompoundBranch(length2, mobile2))
				-> if balanced(mobile2) then
						if length1 * weight1 = length2 * mobileWeight(mobile2) then true
						else false
					else false
			| (CompoundBranch(length1, mobile1), SimpleBranch(length2, weight2))
				-> if balanced(mobile1) then
						if length1 * mobileWeight(mobile1) = length2 * weight2 then true
						else false
					else false
			| (SimpleBranch(length1, weight1), SimpleBranch(length2, weight2))
				-> if length1 * weight1 = length2 * weight2 then true
					else false
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

	let rec sigmaHelper : exp * int -> int
	= fun (exp1, int1) ->
		match exp1 with
			| X -> int1
			| INT(int2) -> int2
			| SIGMA(expf, exps, expt) -> if sigmaHelper(expf, int1) > sigmaHelper(exps, int1) then 0
																	else sigmaHelper(expt, sigmaHelper(expf, int1)) + sigmaHelper(SIGMA(INT(sigmaHelper(expf, int1)+1), exps, expt), int1)
			| ADD(expf, exps) -> sigmaHelper(expf, int1) + sigmaHelper(exps, int1)
			| SUB(expf, exps) -> sigmaHelper(expf, int1) - sigmaHelper(exps, int1)
			| MUL(expf, exps) -> sigmaHelper(expf, int1) * sigmaHelper(exps, int1)
			| DIV(expf, exps) -> sigmaHelper(expf, int1) / sigmaHelper(exps, int1)

  let rec calculator : exp -> int
  = fun exp -> sigmaHelper(exp, 0)
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

	
	let rec lcheck: exp * var list -> bool
	= fun (exp, varList) ->
		match exp with
			| V(var1) -> (match varList with
										| [] -> false
										| hd :: tl -> if hd = var1 then true else lcheck(exp, tl))
			| P(var1, exp1) -> lcheck(exp1,  var1 :: varList)
			| C(exp1, exp2) -> lcheck(exp1, varList) && lcheck(exp2, varList)

	 

  let check : exp -> bool
  = fun exp -> lcheck(exp, [])
end

