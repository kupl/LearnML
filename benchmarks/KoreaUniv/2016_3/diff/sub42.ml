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
	| Var x -> if x = var then Const 1 else Var x
	| Power (x, n) -> if x = var then Times[Const n; Power (x, n-1)] 
			  else Power (x, n)
	| Times l -> 
		begin
		match l with
		| [] -> raise(Failure "Times error")
		| h::t -> Sum[Times ((diff (h,var)) :: t); Times [h; diff (Times t, var)]]
		end
	| Sum l ->
		match l with
		| [] -> raise(Failure "Sum error")
		| h::t -> Sum[diff (h,var) ; diff (Sum t, var)]
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
  = fun mob ->raise NotImplemented(*
	match mob with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> 
		if ((l1*w2) = (l2*w2)) then true else false
	| (SimpleBranch (l1, w1), CompoundBranch (l2, m)) ->
	| (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) ->
	| (CompoundBranch (l1, m1), CompoundBranch (l2, m2) *)
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
	| X -> raise(Failure "Error X...")
	| INT n -> n
	| ADD (e1, e2) -> calculator (e1) + calculator (e2)
	| SUB (e1, e2) -> calculator (e1) - calculator (e2)
	| MUL (e1, e2) -> calculator (e1) * calculator (e2)
	| DIV (e1, e2) -> calculator (e1) / calculator (e2)
	| SIGMA (e1, e2, body) -> 
		let rec calc_body e =
		begin
		match e with
		| X -> calculator (e1)
		| INT n -> n
		| ADD (a1, b1) -> calc_body (a1) + calc_body (b1)
		| SUB (a1, b1) -> calc_body (a1) - calc_body (b1)
		| MUL (a1, b1) -> calc_body (a1) * calc_body (b1)
		| DIV (a1, b1) -> calc_body (a1) / calc_body (b1)
		| SIGMA (a1, b1, body1) -> 
			if ((calc_body a1) = (calc_body b1)) then (calc_body body1)
			else ((calc_body body1) + (calc_body (SIGMA (ADD (a1, INT 1), b1, body1))))
			
		end in
		if ((calculator e1) = (calculator e2)) then (calc_body (body))
		else ((calc_body (body)) 
				+ (calculator (SIGMA (ADD (e1, INT 1), e2, body))))
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

  let rec check_lst lst elem =
	match lst with
	| hd::tl -> if hd = elem then true else check_lst tl elem
	| [] -> false
 
  let check : exp -> bool
  = fun exp ->
	let rec check_helper exp bound_list =
	match exp with
	| P (e1, e2) -> 
		let lis = e1::bound_list in
		check_helper e2 lis
	| V v -> check_lst bound_list v
	| C (e1, e2) -> (check_helper e1 bound_list) && (check_helper e2 bound_list)
	in
	let bound_list = [] in
	check_helper exp bound_list
end
