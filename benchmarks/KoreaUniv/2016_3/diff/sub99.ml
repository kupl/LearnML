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
  = fun (exp, var) ->
	match exp with
	| Const n -> Const 0
	| Var x ->
		if x = var then Const 1 else Const 0
	| Power (x,n) ->
		if x = var then Times [Const n; Power (x,n-1)] else Const 0
	| Times l ->
		(match l with
		| [] -> Const 0
		| hd::[] -> diff (hd, var)
		| hd::tl -> Sum ([Times (diff (hd, var)::tl)]@[Times (hd::[diff (Times (tl), var)])]))
	| Sum l ->
		(match l with
		| [] -> Const 0
		| hd::[] -> diff (hd, var)
		| hd::tl -> Sum ([diff (hd, var)]@[diff (Sum (tl), var)]));;
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

	let rec weight_branch branch =
	match branch with
	| SimpleBranch (l, w) -> w
	| CompoundBranch (l, m) -> weight_mobile (m)

	and weight_mobile mobile =
		match mobile with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
	| (SimpleBranch (l1, w1), CompoundBranch (l2, w2)) -> w1 + weight_branch (CompoundBranch (l2, w2))
	| (CompoundBranch (l1, w1), SimpleBranch (l2, w2)) -> weight_branch (CompoundBranch (l1, w1)) + w2
	| (CompoundBranch (l1, w1), CompoundBranch (l2, w2)) -> weight_branch (CompoundBranch (l1, w1)) + weight_branch (CompoundBranch (l2, w2))

	let check_balance mob =
	match mob with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if w1*l1 = w2*l2 then true else false
	| (SimpleBranch (l1, w1), CompoundBranch (l2, w2)) -> if w1*l1 = (weight_branch (CompoundBranch (l2, w2)))*l2 then true else false
	| (CompoundBranch (l1, w1), SimpleBranch (l2, w2)) -> if (weight_branch (CompoundBranch (l1, w1)))*l1 = w2*l2 then true else false
	| (CompoundBranch (l1, w1), CompoundBranch (l2, w2)) -> if (weight_branch (CompoundBranch (l1, w1)))*l1 = (weight_branch (CompoundBranch (l2, w2)))*l2 then true else false
	
	let rec balanced : mobile -> bool
  = fun mob ->
	match mob with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> check_balance (mob)
	| (SimpleBranch (l1, w1), CompoundBranch (l2, w2)) ->
		if (check_balance (mob) = true)&&(check_balance (w2) = true) then true else false
	| (CompoundBranch (l1, w1), SimpleBranch (l2, w2)) ->
		if (check_balance (mob) = true)&&(check_balance (w1) = true) then true else false
	| (CompoundBranch (l1, w1) , CompoundBranch (l2, w2)) ->
		if (check_balance (mob) = true)&&(check_balance (w1) = true)&&(check_balance (w2) = true) then true else false;;

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

exception Error

  let rec calculator : exp -> int
  = fun exp ->
	match exp with
	| X -> raise Error
	| INT (n) -> n
	| ADD (exp1, exp2) -> calculator (exp1) + calculator (exp2)
	| SUB (exp1, exp2) -> calculator (exp1) - calculator (exp2)
	| MUL (exp1, exp2) -> calculator (exp1) * calculator (exp2)
	| DIV (exp1, exp2) -> calculator (exp1) / calculator (exp2)
	| SIGMA (exp1, exp2, exp3) ->
		if exp1 = X then raise Error else
			if calculator (exp1) = calculator (exp2) then cal_n (exp3, calculator (exp1)) else cal_n (exp3, calculator (exp1)) + calculator (SIGMA (INT (calculator (exp1) + 1), exp2, exp3))

	and cal_n (exp, n) =
	match exp with
	| X -> n
	| INT (m) -> m
	| ADD (exp1, exp2) -> (cal_n (exp1, n)) + (cal_n (exp2, n))
	| SUB (exp1, exp2) -> (cal_n (exp1, n)) - (cal_n (exp2, n))
	| MUL (exp1, exp2) -> (cal_n (exp1, n)) * (cal_n (exp2, n))
	| DIV (exp1, exp2) -> (cal_n (exp1, n)) / (cal_n (exp2, n))
  | SIGMA (exp1, exp2, exp3) ->
		if exp1 = X then raise Error else
			if calculator (exp1) = calculator (exp2) then cal_n (INT (cal_n (exp3, calculator (exp1))), n) else cal_n (INT (cal_n (exp3, calculator (exp1))), n) + cal_n (SIGMA (INT (calculator (exp1) + 1), exp2, exp3), n);;

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

	let rec compare_var a b =
	match b with
	| [] -> false
	| hd::tl -> if hd = a then true else compare_var a tl

	let rec make_l exp l =
	match exp with
	| V (v) -> compare_var v l
	| P (v, e) -> make_l e (l@[v])
	| C (e1, e2) -> (make_l e1 l)&&(make_l e2 l)

  let check : exp -> bool
  = fun exp ->
	make_l exp [];;

end

