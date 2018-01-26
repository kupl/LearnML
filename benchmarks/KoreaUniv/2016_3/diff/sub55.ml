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
  		| Var v -> 
  			if v = var then Const 1
  			else Const 0
  		| Power (x, n) -> 
			if x = var then 
				if n = 0 then Const 0
				else if n = 1 then Const 1
				else Times [ Const n; Power (x, n-1) ]
			else Const 0
		| Times l ->
			(
				match l with
				| [] -> Const 0
				| hd::[] -> diff(hd, var) 
				| hd::tl -> 
					match hd with
					| Const 1 ->
					diff (Times tl, var)
					| Const n ->
						Times [ hd; diff ( (Times tl), var ) ]
					| _ -> 
						Sum [Times (diff(hd, var)::tl); Times [ hd; diff(Times tl, var)]]
				)
		| Sum l ->
			match l with
			| [] -> Const 0
			| hd::[] -> (diff (hd, var))
			| hd::tl -> Sum [(diff (hd, var)); (diff ((Sum tl),var))]
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

	let rec eval1 : mobile -> int
		= fun ev -> 
			match ev with
			| (SimpleBranch(a,b), SimpleBranch(c,d)) ->
				if a*b=c*d then b+d else -1
			| (CompoundBranch(a,b), SimpleBranch(c,d)) ->
				eval1(SimpleBranch(a, eval1(b)), SimpleBranch(c,d))
			| (SimpleBranch(a,b), CompoundBranch(c,d)) ->
				eval1(SimpleBranch(a,b), SimpleBranch(c, eval1(d)))
			| (CompoundBranch(a,b), CompoundBranch(c,d)) ->
				eval1(SimpleBranch(a, eval1(b)), SimpleBranch(c, eval1(d)))

  let balanced : mobile -> bool
  = fun mob ->  
  	if eval1(mob) = ( -1 )
  		then false
  	else true(* TODO *)
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

exception Fault

let rec eval2 (exp, xval) =
	match (exp, xval) with
	 | (X, INT x) -> x
	 | (X, _) -> raise Fault 
	 | (INT i, _) -> i
	 | (ADD (exp1, exp2), _) ->
	 	(
		match exp1, exp2 with
		| INT x, INT y -> x+y
		| _, _ -> (eval2 (exp1, xval) + eval2 (exp2, xval))
		)
	| (SUB (exp1, exp2), _) ->
		(           
		match exp1, exp2 with                   
		| INT x, INT y -> x-y
		| _, _ -> (eval2 (exp1, xval) - eval2 (exp2, xval))
		)
	| (MUL (exp1, exp2), _) ->
		(
		match exp1, exp2 with                       
		| INT x, INT y -> x*y
		| _, _ -> (eval2 (exp1, xval) * eval2 (exp2, xval))
		)                                           
	| (DIV (exp1, exp2), _) ->                                                      
		(                                                                       
		match exp1, exp2 with                                                                                       
		| INT x, INT y -> x/y  
		| _, _ -> (eval2 (exp1, xval) / eval2 (exp2, xval))      
		)                                     
	| (SIGMA (exp1, exp2, exp3), _) ->
		let x = eval2 (exp1, xval)
		and y = eval2 (exp2, xval) in
		(
		if x>y then 0
		else if x=y then eval2 (exp3, INT x)
		else eval2 (exp3, INT x) + eval2 (SIGMA(INT (x+1), INT y, exp3), xval)
		)

  let calculator : exp -> int
  = fun exp -> eval2 (exp, X)
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

	let check : exp -> bool
	=fun exp ->
		let rec check_help m n_list = 
		match m with
		| V n -> List.mem n n_list
		| P (n, m1) -> check_help m1 (n::n_list)
		| C (m1,m2) -> check_help m1 n_list && check_help m2 n_list
	in check_help exp []
end

