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

  let rec map f (l,var) =
  	match l with
  	| [] -> []
  	| hd::tl -> (f (hd,var))::(map f (tl,var))

  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
 	match exp with
 	| Const n -> Const 0
 	| Var x -> if x <> var then Const 0 else Const 1
 	| Power(x, n) -> if x <> var then Const 0 else Times [Const n; Power(x, (n-1))]
 	| Times lst ->
 		begin
	 		match lst with
	 		| [] -> Const 0
	 		| hd::tl -> Sum [Times ((diff (hd,var))::tl);Times [hd;diff ((Times tl),var)]]
 		end
 	| Sum lst -> Sum (map diff (lst,var))
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

  let rec mobileVal : mobile -> int
  = fun mob ->
  	match mob with
  	| (SimpleBranch(ll, lw), SimpleBranch(rl, rw)) -> lw+rw
   	| (CompoundBranch(ll, lm), SimpleBranch(rl, rw)) -> (mobileVal lm)+rw
  	| (SimpleBranch(ll, lw), CompoundBranch(rl, rm)) -> lw+(mobileVal rm)
  	| (CompoundBranch(ll, lm), CompoundBranch(rl, rm)) -> (mobileVal lm) + (mobileVal rm)

  let rec balanced : mobile -> bool
  = fun mob -> 
  	match mob with
  	| (SimpleBranch(ll, lw), SimpleBranch(rl, rw)) -> (ll*lw) = (rl*rw)
  	| (CompoundBranch(ll, lm), SimpleBranch(rl, rw)) -> (balanced lm) && (((mobileVal lm)*ll) = (rl*rw))
  	| (SimpleBranch(ll, lw), CompoundBranch(rl, rm)) -> ((ll*lw) = (rl*(mobileVal rm))) && (balanced rm)
  	| (CompoundBranch(ll, lm), CompoundBranch(rl, rm)) -> (balanced lm) && (balanced rm) && ((ll*(mobileVal lm)) = (rl*(mobileVal rm)))
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

  let rec convertVar : exp -> int -> exp
  = fun exp i ->
  	match exp with
  	| X -> INT i
  	| INT n -> INT n
  	| ADD (exp1, exp2) -> ADD ((convertVar exp1 i), (convertVar exp2 i))
  	| SUB (exp1, exp2) -> SUB ((convertVar exp1 i), (convertVar exp2 i))
  	| MUL (exp1, exp2) -> MUL ((convertVar exp1 i), (convertVar exp2 i))
  	| DIV (exp1, exp2) -> DIV ((convertVar exp1 i), (convertVar exp2 i))
  	| SIGMA (exp1, exp2, exp3) -> SIGMA ((convertVar exp1 i), (convertVar exp2 i), exp3)	(*아리까리 함*)

  let rec calculator : exp -> int
  = fun exp -> 
  	match exp with
  	| X -> raise NotImplemented
  	| INT i -> i
  	| ADD (exp1, exp2) -> (calculator exp1) + (calculator exp2)
  	| SUB (exp1, exp2) -> (calculator exp1) - (calculator exp2)
  	| MUL (exp1, exp2) -> (calculator exp1) * (calculator exp2)
  	| DIV (exp1, exp2) -> (calculator exp1) / (calculator exp2)
  	| SIGMA (i, j, exp) ->
  		let start = calculator i in
  			if (calculator j) < start 
  				then raise NotImplemented
  			else if (calculator j) = start
  				then calculator (convertVar exp start)
  			else 
  				(calculator (convertVar exp start)) + (calculator (SIGMA(ADD (i, INT 1), j, exp)))

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

  let rec eval : exp -> var list -> bool
  = fun exp lst ->
  	match exp with
  	| V var -> 
	  	begin
	  		match lst with
	  		|[] -> false
	  		|hd::tl -> if var = hd then true else eval exp tl
	  	end
	| P (var, exp2) -> eval exp2 (var::lst)
	| C (exp1, exp2) -> (eval exp1 lst)&&(eval exp2 lst)


  let check : exp -> bool
  = fun exp -> 
  	let state = [] in
  		match exp with
  		| V var -> eval exp state
  		| P (var, exp) -> eval exp (var::state)
  		| C (exp1, exp2) -> (eval exp1 state)&&(eval exp2 state)
end

