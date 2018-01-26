(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)



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


let rec diff (exp, var) =
	match exp with
	| Const a -> Const 0
	| Var x when x = var -> Const 1
	| Var y -> Const 0
	| Power (str, a) ->
		(
		if str = var then
			(match a with
			| 0 -> Const 0
			| 1 -> Const 1
			| 2 -> Times [Const 2; Var var]
			| _ -> let aa = a-1 in Times [Const a ; Power (str, aa)]
			)
		else Const 0
		) 
	| Times ls ->
		(
		match ls with
		| [] -> Const 0
		| [hd] -> diff (hd, var)
		| hd :: [tl] -> if hd = Const 0 || tl = Const 0 then Const 0
										else Sum [Times [diff (hd, var); tl]; Times [hd; diff (tl, var)]]
		| hd :: tl -> if hd = Const 0  then Const 0
									else Sum [Times [diff (hd, var); Times tl]; Times [hd; diff (Times tl, var)]]
		)
	| Sum ls ->
		(
		match ls with
		| [] -> Const 0
		| [hd] -> diff (hd, var)
		| hd :: [tl] -> if hd = Const 0 then diff (tl, var)
										else if tl = Const 0 then diff (hd, var)
										else Sum [diff (hd, var); diff (tl, var)]
		| hd :: tl -> if hd = Const 0 then diff (Sum tl, var)
									else Sum [diff (hd, var); diff (Sum tl, var)]
		)



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

	let rec wei (m)  =
		match m with
		| SimpleBranch (_, w) -> w
		| CompoundBranch (_, (mo1, mo2)) -> wei mo1 + wei mo2

	let pow (m) =
		match m with
		| SimpleBranch (l, w) -> l * w
		| CompoundBranch (l, (mo1, mo2)) -> l * (wei mo1 + wei mo2)

	let rec balanced : mobile -> bool
	= fun mob ->
		match mob with
		| (m1, m2) -> (
									(if pow (m1) = pow (m2) then true else false) &&
										(
										match m1, m2 with
										| (SimpleBranch(_,_),SimpleBranch(_,_)) -> true
										| (CompoundBranch(_,k1), CompoundBranch(_,k2)) -> balanced k1 && balanced k2
										| (SimpleBranch(_,_),CompoundBranch(_,k2)) -> balanced k2
										| (CompoundBranch(_,k1),SimpleBranch(_,_)) -> balanced k1
										)
									)

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
	= fun exp ->
		match exp with
		| X -> 0
		| INT a -> a
		| ADD (a, b) -> (calculator a) + (calculator b)
		| SUB (a, b) -> (calculator a) - (calculator b)
		| MUL (a, b) -> (calculator a) * (calculator b)
		| DIV (a, b) -> (calculator a) / (calculator b)
		| SIGMA (a, b, ex) -> let aa = calculator a and bb = calculator b in
													if aa > bb then 0 else chec (ex, a) + calculator (SIGMA (ADD(a, INT 1), b, ex))
	and chec (ex, x) =
		match ex with
		| X -> calculator x
		| INT a -> a
		| ADD (a1, a2) -> chec (a1, x) + chec (a2, x)
		| SUB (a1, a2) -> chec (a1, x) - chec (a2, x)
		| MUL (a1, a2) -> chec (a1, x) * chec (a2, x)
		| DIV (a1, a2) -> chec (a1, x) / chec (a2, x)
		| SIGMA (i, j, a1) -> calculator (SIGMA (INT (chec (i, x)), INT (chec (j, x)), a1))

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
	= fun exp ->
		match exp with
		| V (_) -> false
		| P (a, e) -> let l = cklist e in
								(match l with
								| [] -> true
								| hd :: tl -> if mtlist(a, l) then if remlist (a,l) = [] then true else
																										 (match e with
																										| V (_) -> true
																										| P (_,_) -> check e
																										| C (V (a), V (b)) -> true
																										| C (V (_), e1) -> check e1
																										| C (e1, V (_)) -> check e1
																										| C (e1, e2) -> check e)
															else false)
		| C (e1, e2) -> check e1 && check e2
	and cklist exp =
		match exp with
		| V (a) -> [a]
		| P (a, e) -> cklist e
		| C (e1, e2) -> cklist e1 @ cklist e2
	and mtlist (a, l) =
		match l with
		| [] -> false
		| hd :: tl -> if hd = a then true else mtlist (a, tl)
	and remlist (a, l) =
		match l with
		| [] -> []
		| hd :: tl -> if hd = a then remlist (a, tl) else [a] @ remlist (a, tl)

end

