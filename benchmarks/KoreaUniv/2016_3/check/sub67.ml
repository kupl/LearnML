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
		| Var str -> if str = var then Const 1
					else Const 0
		| Power (str, n) -> 
			if str = var then Times [Const n; Power(str, n - 1)] 
			else Const 0
		| Times lst -> 
			begin
			match lst with 
			| [] -> Const 0
			| hd::tl -> Sum [Times ((diff (hd, var))::tl);Times[hd;diff((Times tl),var)]]
			end
		| Sum lst ->
			match lst with
			| [] -> Const 0
			| hd::tl -> Sum [diff (hd, var); diff((Sum tl), var)]
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

	let rec sum : mobile -> int
	= fun mob ->
		match mob with
		| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
		| (SimpleBranch (l1, w1), CompoundBranch (l2, mobile)) -> w1 + (sum mobile) 
		| (CompoundBranch (l1, mobile), SimpleBranch (l2, w2)) -> (sum mobile) + w2
		| (CompoundBranch (l1, mobile1), CompoundBranch (l2, mobile2)) -> (sum mobile1) + (sum mobile2) 

  let rec balanced : mobile -> bool
  = fun mob -> 
		match mob with
		| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> 
			if (l1 * w1) = (l2 * w2) then true else false
		| (SimpleBranch (l1, w1), CompoundBranch (l2,  mobile2)) ->
			if (l1 * w1) = (l2 * (sum mobile2)) then true else false
		| (CompoundBranch (l1, mobile1), SimpleBranch (l2, w2)) ->
			if (l1 * (sum mobile1)) = (l2 * w2) then true else false
		| (CompoundBranch (l1, mobile1), CompoundBranch (l2, mobile2)) ->
			if (l1 * (sum mobile1)) = (l2 * (sum mobile2)) then true else false
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

  let rec calculator2 : exp * int -> int
  = fun (exp, t) -> 
		match exp with
		| X -> t
		| INT n1-> n1
		| ADD (n1, n2) -> (calculator2  (n1, t)) + (calculator2 (n2, t))
		| SUB (n1, n2) -> (calculator2 (n1, t)) - (calculator2 (n2, t))
		| MUL (n1, n2) -> (calculator2 (n1, t)) * (calculator2 (n2, t))
		| DIV (n1, n2) -> (calculator2 (n1, t)) / (calculator2 (n2, t))
	 	| SIGMA (n1, n2, n3) ->
			if (calculator2 (n1, t)) > (calculator2 (n2, t)) then 0
			else calculator2 (n3, (calculator2 (n1, t))) + calculator2 (SIGMA ((INT (calculator2 (n1, t) + 1)), n2, n3), 0) 

	let calculator : exp -> int
	= fun exp -> calculator2 (exp, 0)
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


  let rec compare : exp -> string list -> bool
  = fun exp lst ->
    match exp with
    | V x -> 
			begin
      match lst with
      | [] -> false
      | hd::tl -> if x = hd then true
                  else compare exp tl
			end
    | P (x, exp1) -> compare exp1 (x::lst)
		| C (exp1, exp2) -> (compare exp1 lst) && (compare exp2 lst)


  let rec check : exp -> bool
  = fun exp ->
		let lst = [] in   
		match exp with
		| V x -> false
		| P (x, exp1) -> compare exp1 (x::lst)
		| C (exp1, exp2) -> (compare exp1 lst) && (compare exp2 lst)

end

