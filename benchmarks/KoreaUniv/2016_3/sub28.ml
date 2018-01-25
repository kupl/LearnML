(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented
exception InvalidExpression

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

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
	| Const n -> Const 0
	| Var a -> if a = var then Const 1
						 else Const 0
	| Power (a, n) -> 
	begin 
		match n with
		| 0 -> Const 0
		| 1 -> if a = var then Const 1
					 else Const 0
		| 2 -> if a = var then Times [Const 2; Var a]
					 else Const 0
		| _ -> if a = var then Times [Const n; Power (a, n-1)]
					 else Const 0
	end
	| Times lst ->
	begin 
		match lst with
		| [] -> Const 0
		| hd :: tl -> match tl with
			| [] -> diff (hd, var)
			| thd :: [] -> (Sum [Times[diff (hd, var); thd]; Times[hd; diff(thd, var)]])
			| _ -> (Sum [Times[diff (hd, var); Times tl]; Times[hd; diff(Times tl, var)]])
	end
	| Sum lst -> 
		match lst with
		| [] -> Const 0
		| hd :: tl -> match tl with
			| [] ->  diff (hd, var)
			| thd :: [] -> (Sum [diff (hd, var); diff (thd, var)])
			| _ -> Sum ([diff (hd, var); diff (Sum tl, var)]);;
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

  let rec balanced : mobile -> bool
  = fun mob -> let rec fweight : mobile -> int = fun fw -> match fw with
			| (SimpleBranch (l1, w1), SimpleBranch (l2, w2))
			-> if l1 * w1 = l2 * w2 then w1 + w2
				 else -1000000
			| (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) 
			-> if l1 * w1 = l2 * (fweight m2) then w1 + (fweight m2)
				 else -1000000
			| (CompoundBranch (l1, m1), SimpleBranch (l2, w2))
			-> if l1 * (fweight m1) = l2 * w2 then (fweight m1) + w2
				 else -1000000
			| (CompoundBranch (l1, m1), CompoundBranch (l2, m2))
			-> if l1 * (fweight m1) = l2 * (fweight m2) then (fweight m1) + (fweight m2)
				 else -1000000 in
	match mob with
	| (SimpleBranch (l11, w11), SimpleBranch (l22, w22)) 
	-> l11 * w11 = l22 * w22
	| (SimpleBranch (l11, w11), CompoundBranch (l22, m22)) 
	-> l11 * w11 = l22 * (fweight m22)
	| (CompoundBranch (l11, m11), SimpleBranch (l22, w22)) 
	-> l11 * (fweight m11) = l22 * w22
	| (CompoundBranch (l11, m11), CompoundBranch (l22, m22)) 
	-> l11 * (fweight m11) = l22 * (fweight m22);;
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

  let rec calculator : exp -> int
  = fun exp -> match exp with
	| X -> raise InvalidExpression
	| INT n -> n
	| ADD (n1, n2) -> (calculator (n1)) + (calculator (n2))
	| SUB (n1, n2) -> (calculator (n1)) - (calculator (n2))
	| MUL (n1, n2) -> (calculator (n1)) * (calculator (n2))
	| DIV (n1, n2) -> (calculator (n1)) / (calculator (n2))
	| SIGMA (nf, ne, e1) -> 
			let rec expfun : exp * exp -> exp = fun (f1, v1) -> match f1 with
				| X -> v1
				| INT nn -> INT nn
				| ADD (exp1, exp2) -> ADD(expfun (exp1, v1), expfun (exp2, v1))
				| SUB (exp1, exp2) -> SUB(expfun (exp1, v1), expfun (exp2, v1))
				| MUL (exp1, exp2) -> MUL(expfun (exp1, v1), expfun (exp2, v1))
				| DIV (exp1, exp2) -> DIV(expfun (exp1, v1), expfun (exp2, v1))
				| SIGMA (exp1, exp2, exp3) -> SIGMA (expfun (exp1, v1), expfun (exp2, v1), expfun (exp3, v1)) in 
		if (calculator (nf)) = (calculator (ne)) then calculator(expfun (e1, nf))
		else (calculator(expfun (e1, ne))) + (calculator (SIGMA (nf, SUB (ne, INT 1), e1)));; 
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

  let rec check : exp -> bool
  = fun exp -> let rec checklst : exp * var list -> bool
		= fun (ee, llst) -> match ee with
			| V vf ->
			begin
			  match llst with 
				| [] -> false
				| hd :: tl-> if hd = vf then true
										 else checklst ((ee), tl)
			end
			| P (vf, expf) -> checklst (expf, vf::llst)
			| C (expf1, expf2) -> (checklst (expf1, llst)) && (checklst (expf2, llst)) in
		let varlist = [] in
	match exp with 
	| P (v1, exp1) -> checklst (exp1, v1::varlist)
	| _ -> checklst (exp, varlist);;
end
