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
		match (exp, var) with
		| (Const n, v) -> Const 0
		| (Var x, v) -> if x = v then Const 1 else Const 0
		| (Power (v1, n), v) -> if v1 = v then Times [Const n; Power(v1, n-1)] else Const 0
		| (Times l, v) -> (match l with
			| (Const n :: tl) -> Times ((Const (n*(iter v l)))::tl)
			| _ -> Const 0)
		| (Sum l, v) -> (match l with
				| [] -> Const 0
		  	| (hd::tl) -> Sum ((diff (hd, v))::((diff ((Sum tl), v))::[])))
	
	and count x e =
		match e with
		| Const n -> 0
		| Var v -> if x = v then 1 else 0
		| Power (v,n) -> if v = x then n else 0
		| Times l -> iter x l
		| Sum l -> 0
	and iter x l =
 		match l with
		| [] -> 0
		| hd::tl -> (count x hd) + (iter x tl)

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
  = fun mob ->
		match mob with
		| (br, bl) -> if (torque br) = (torque bl) then true else false
	and torque : branch -> int
	= fun b ->
		match b with
		| SimpleBranch (l, w) -> l*w
		| CompoundBranch (l, m) ->
			if balanced m then
				(weight m)*l else -999
	and weight : mobile -> int
	= fun m ->
		match m with
		| (br, bl) -> 
			(match (br, bl) with
			 | (SimpleBranch (l, w), SimpleBranch (l2,w2)) -> w + w2
			 | (SimpleBranch (l, w), CompoundBranch (l2, m)) -> w + (weight m)
			 | (CompoundBranch (l, m), SimpleBranch (l2, w)) -> (weight m) + w
			 | (CompoundBranch (l, m), CompoundBranch (l2, m2)) -> (weight m) + (weight m2)) 

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
		| X -> 375
		| INT n -> n
		| ADD (e1, e2) ->
			let v1 = calculator e1 in
			let v2 = calculator e2 in
				(match v1,v2 with
				 |  n1,  n2 -> (n1 + n2))
		| SUB (e1, e2) ->
			let v1 = calculator e1 in
			let v2 = calculator e2 in
				(match v1, v2 with
				 | n1, n2 -> (n1 - n2))
		| MUL (e1, e2) ->
			let v1 = calculator e1 in
			let v2 = calculator e2 in
				(match v1, v2 with
				 | n1, n2 -> (n1 * n2))
		| DIV (e1, e2) ->
			let v1 = calculator e1 in
			let v2 = calculator e2 in
				(match v1, v2 with
				 |  n1, n2 -> (n1/n2))
		| SIGMA (e1, e2, e3) -> 375
				

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
		| V v -> true
		| P (v, e) -> true
		| C (e1, e2) -> true

end
