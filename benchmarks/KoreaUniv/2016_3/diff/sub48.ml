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
	| Const x -> Const 0;
	| Var v -> if var = v then Const 1 else Const 0
	| Power (v , i) -> if var = v then Times [Const i; Power(v, (i-1))]
																else Const 0
	| Times lt -> (match lt with | [] -> Const 0
| hd::tl -> Sum[(Times ((diff (hd, var))::tl)); Times [hd;(diff ((Times tl),var))]])
	| Sum lst ->
					 match lst with | [] -> Const 0
					| hd::tl -> Sum [(diff (hd, var)); (diff ((Sum tl), var))]
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

	let rec checkb : mobile -> int = fun i ->
	match i with
	| (SimpleBranch (l1,w1), SimpleBranch(l2,w2)) -> (w1+w2)
	| (SimpleBranch (l1,w1), CompoundBranch(l2,(c,d)))->(w1 + checkb (c,d))
	| (CompoundBranch (l1,(c,d)), SimpleBranch(l2,w2))->(w2 + checkb (c,d))
	| (CompoundBranch (l1, (c1,d1)),CompoundBranch(l2,(c2,d2))) ->
		(checkb(c1,d1) + checkb(c2,d2))
		
  let rec balanced : mobile -> bool
  = fun mob -> match mob with
	| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> ((l1*w1) = (l2*w2))
	| (SimpleBranch (l1, w1), CompoundBranch (l2, (a, b))) -> 
	((balanced (a,b)) && ((l1*w1) = (l2*(checkb(a,b)))))
	| (CompoundBranch (l1, (a, b)), SimpleBranch (l2, w2)) ->
	((balanced (a,b)) && ((l2*w2) = (l1*(checkb(a, b)))))
	| (CompoundBranch (l1, (a1, b1)), CompoundBranch (l2, (a2, b2))) -> 
	((balanced (a1, b1)) && (balanced (a2, b2)) &&
	((l1*(checkb(a1,b1))) = (l2*(checkb (a2,b2)))))
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
	
	let rec xchange : (exp * exp) -> exp = fun (xexp, xch) -> 
	match xexp with
	| X -> xch
	| INT i -> INT i
	| ADD (e1, e2) -> ADD(xchange(e1, xch), xchange(e2, xch))
	| SUB (e1, e2) -> SUB(xchange(e1, xch), xchange(e2, xch))
	| MUL (e1, e2) -> MUL(xchange(e1, xch), xchange(e2, xch))
	| DIV (e1, e2) -> DIV(xchange(e1, xch), xchange(e2, xch))
	| SIGMA (e1, e2, e3) -> SIGMA(xchange(e1, xch), xchange(e2, xch), e3)
	
  let rec calculator : exp -> int
  = fun exp -> match exp with
	|	X -> raise NotImplemented
	| INT i -> i
	| ADD (e1, e2) -> ((calculator e1) + (calculator e2))
	| SUB (e1, e2) -> ((calculator e1) - (calculator e2))
	| MUL (e1, e2) -> ((calculator e1) * (calculator e2))
	| DIV (e1, e2) -> ((calculator e1) / (calculator e2))
	| SIGMA (e1, e2, e3) -> if ((calculator e1) = (calculator e2)) 
				then (calculator (xchange(e3, e1)))
				else ((calculator (SIGMA((INT ((calculator e1)+1)), e2, e3)))
								+(calculator (xchange(e3,e1))))
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

	let rec xfind : (string list * string) -> bool = fun (en, x) ->
	match en with
	| [] -> false
	| hd::tl -> if (hd = x) then true else ((xfind(tl,x)) ||false)

  let rec find : exp * string list -> bool = fun (exp, env) ->
	match exp with
	| V x -> xfind (env, x)
	| P (x, y) -> find(y, x::env)
	| C (x, y) -> find(x, env) && find(y, env)
	
	let rec check : exp -> bool = fun exp -> 
	find (exp,[])	
end

