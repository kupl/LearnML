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
  = fun (exp, var) ->  (* TODO *)
	match exp with
	| Const(a) -> Const 0
	| Var a when a=var -> Const 1
	| Power(a,b) when var=a -> Times[Const b; Power(a, b-1)]
	| Times l -> (match l with
						|	hd::tl when diff(hd,var)=Const 0 -> (match hd with
																									| Const a -> Times[hd; diff(Times tl, var)]
																									| _-> Times[Const 0; diff(Times tl, var)])
						| hd::tl -> Times[diff(hd, var); diff(Times tl, var)] 
						| [] -> Const 1)
	| Sum l -> (match l with
						| hd::tl -> Sum[diff(hd, var); diff(Sum tl, var)]
						| [] -> Const 0)
	| _->Const 0					
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
  = fun mob -> (* TODO *)
	match mob with
	| SimpleBranch(a,b),SimpleBranch(c,d) when a*b = c*d -> true
	| SimpleBranch(a,b),CompoundBranch(c,d) when a*b = c*weight(d) && balanced(d) -> true
	| CompoundBranch(a,b),SimpleBranch(c,d) when a*weight(b)=c*d && balanced(b) -> true
	| CompoundBranch(a,b),CompoundBranch(c,d) when a*weight(b)=c*weight(d) && balanced(b) && balanced(d) -> true   
	| _->false
	
and weight : mobile -> int
=fun mob ->
match mob with
| SimpleBranch(a,b), SimpleBranch(c,d) -> b+d
| SimpleBranch(a,b), CompoundBranch(c,d) -> b+ weight(d)
| CompoundBranch(a,b), SimpleBranch(c,d) -> weight(b)+d
| CompoundBranch(a,b), CompoundBranch(c,d) -> weight(b)+weight(d)
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
  = fun exp ->   (* TODO *)
	match exp with
	| INT n -> n
	| ADD (a,b) -> (calculator a)+(calculator b)
	| DIV (a,b) when calculator(b)=0 -> raise (NotImplemented) 
	| DIV (a,b) -> (calculator a)/ (calculator b)
	| MUL (a,b) -> (calculator a)*(calculator b)
	| SUB (a,b) -> (calculator a)-(calculator b)
	| SIGMA (a,b,c) when (calculator a)>(calculator b) -> raise(NotImplemented)
	| SIGMA (a,b,c) when (calculator a) = (calculator b) -> substi(c, calculator a) 
	| SIGMA (a,b,c) -> substi(c,calculator a) + (calculator (SIGMA(ADD(a, INT 1), b, c)))
	| _ -> raise(NotImplemented)
and substi : exp*int -> int
= fun exp ->
match exp with 
| X,a -> a
| ADD(a,b),c -> substi(a,c) + substi(b,c)
| DIV(a,b),c -> substi(a,c) / substi(b,c)
| MUL(a,b),c -> substi(a,c) * substi(b,c)
| SUB(a,b),c -> substi(a,c) - substi(b,c)
| exp,a -> calculator exp
end

(*********************)
(*     Problem 4 	   *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check : exp -> bool
  = fun exp ->  (* TODO *)
	if last(varcheck(exp), exp) then true else false
and helpcheck : exp -> exp list
= fun exp ->
match exp with
| P(a,b) -> (V a)::helpcheck(b)
| C(a,b) -> helpcheck(a)@helpcheck(b)
| V(a) -> []
and varcheck : exp -> exp list
= fun exp ->
match exp with
| V(a) -> (V a)::[]
| C(a,b) -> varcheck(a)@varcheck(b)
| P(a,b) -> varcheck(b)
and findvar : exp -> exp list
= fun exp ->
match exp with 
| V(a) -> (V a)::[]
| P(a,b) -> (V a)::findvar(b)
| C(a,b) -> findvar(a)@findvar(b)
and helpcheck2 : exp * exp -> exp list
= fun (ex, exp) ->
match ex with
| P(a,b) when b=exp -> (V a)::helpcheck2(b,exp)
| P(a,b) -> (V a)::helpcheck2(b,exp)
| C(a,b) when a=exp -> []
| C(a,b) when b=exp -> helpcheck2(a,exp)
| C(a,b) -> helpcheck2(a,exp)@helpcheck2(b,exp)
| V(a) -> []
and confi : exp list * exp -> bool
= fun (a, b) ->
match a with
|hd::tl when hd = b -> true 
|hd::tl -> confi(tl,b) 
|_ -> false
and last : exp list * exp -> bool
= fun (a,b) ->
match a with
| hd::tl -> confi(helpcheck2(b, hd), hd) && last(tl, b)
| [] -> true
end

