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

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
	| Const n -> Const 0
	| Var v -> if v = var then Const 1 else Const 0
	| Power (v, n) -> if v = var then Times [Const n; Power (var, n-1)] else Const 0
	| Times [] -> Const 0
	| Times [a] -> diff (a, var) (*For Simpler Result*)
	| Times (hd::tl) -> Sum [Times (diff (hd, var)::tl) ; Times [hd ; diff (Times tl, var)]]
	| Sum [] -> Const 0
	| Sum [a] -> diff (a,var) (*For Simpler Result*)
	| Sum (hd::tl) -> Sum [diff (hd,var) ; diff (Sum tl,var)]
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
	
	let rec totalWeight : mobile -> int
	= fun mob -> match mob with
	| (SimpleBranch (l1, w1) , SimpleBranch (l2, w2)) -> 		 w1 + w2
	| (SimpleBranch (l1, w1) , CompoundBranch (l2, mob2)) -> w1 + (totalWeight mob2)
	| (CompoundBranch (l1, mob1) , SimpleBranch (l2, w2)) -> w2 + (totalWeight mob1)
	| (CompoundBranch (l1, mob1) , CompoundBranch (l2, mob2)) -> (totalWeight mob1) + (totalWeight mob2)

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
	| (SimpleBranch (l1, w1) , SimpleBranch (l2, w2)) ->		 (l1 * w1 = l2 * w2)
	| (SimpleBranch (l1, w1) , CompoundBranch (l2, mob2)) -> (l1 * w1 = l2 * (totalWeight mob2)) && (balanced mob2)
	| (CompoundBranch (l1, mob1) , SimpleBranch (l2, w2)) -> (l2 * w2 = l1 * (totalWeight mob1)) && (balanced mob1)
	| (CompoundBranch (l1, mob1), CompoundBranch (l2, mob2)) -> 
		(l1 * (totalWeight mob1) = l2 * (totalWeight mob2)) && (balanced mob1) && (balanced mob2)
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
	| X -> raise (Failure "X is not calculated")
	| INT n -> n
	| ADD (a, b) -> (calculator a) + (calculator b)
	| SUB (a, b) -> (calculator a) - (calculator b)
	| MUL (a, b) -> (calculator a) * (calculator b)
	| DIV (a, b) -> if (calculator b) = 0 then raise (Failure "Divided by 0")
									else (calculator a) / (calculator b)
	| SIGMA (a, b, f) ->
		let n1 = calculator a in
		let n2 = calculator b in
			if n1 > n2 then 0
			else if n1 = n2 then evalX (f,n1)
			else evalX(f,n1) + calculator (SIGMA (INT (n1 + 1), INT n2, f))
	
	and	evalX : exp * int -> int 
	= fun (f, x) -> match f with
	| X -> x
	| INT n -> n
	| ADD (a,b) -> evalX (a,x) + evalX (b,x)
	| SUB (a,b) -> evalX (a,x) - evalX (b,x)
	| MUL (a,b) -> evalX (a,x) * evalX (b,x)
	| DIV (a,b) -> if evalX (b,x) = 0 then raise (Failure "Divied by 0")
									else evalX (a,x) / evalX (b,x)
	| SIGMA (a, b, c) -> 
		let n1 = evalX (a,x) in
		let n2 = evalX (b,x) in
			calculator (SIGMA (INT n1, INT n2, c))
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
	
	let rec checkEnv : exp * var list -> bool
	= fun (exp, env) -> match (exp, env) with
	| (V _, []) -> false
	| (V var, hd::tl) -> if var = hd then true else checkEnv (V var, tl)
	| (P (var, exp), env) -> checkEnv (exp, var::env)
	| (C (e1, e2), env) -> checkEnv (e1, env) && checkEnv (e2, env)

  let check : exp -> bool
  = fun exp -> checkEnv (exp, [])
end

