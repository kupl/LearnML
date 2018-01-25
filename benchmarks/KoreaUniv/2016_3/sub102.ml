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
		| Const num-> Const 0
		| Var v -> if v=var then Const 1 else Const 0
		| Power (v,p) -> 
			if v <> var then Const 0
			else begin
				if p = 0 then Const 0
				else if p = 1 then Const 1
				else Times([Const p ; Power (v, p-1)])
			end
		| Times lst  ->
			begin
				match lst with
				| hd::tl ->  if tl=[] then diff(hd,var)
                     else Sum([Times(diff(hd,var)::tl);Times([hd;diff(Times(tl),var)])])
				| _ -> Const 1
			end
		| Sum lst ->
			begin	
				match lst with
			 |		hd::tl ->  Sum ([diff(hd,var);diff(Sum(tl),var)])
			 | _ -> Const 0
			end
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

	let rec get_sum : mobile -> int
	= fun mob ->
		match mob with
		| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
 	  | (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> w1 + get_sum(m2)
    | (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) -> get_sum(m1) + w2
    | (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) -> get_sum(m1) + get_sum(m2)

  let rec balanced : mobile -> bool
  = fun mob ->
		match mob with
		| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> if l1*w1 = l2*w2 then true else false
		| (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) ->
			if balanced m2 then
			begin
				if get_sum(m2) * l2 = l1 * w1 then true else false
			end
			else false
		| (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) ->
			if balanced m1 then
			begin	
				if get_sum(m1) * l1 = l2 * w2 then true else false
			end
			else false
		| (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) ->
			if balanced m1 && balanced m2 then
			begin
				if get_sum(m1) * l1 = get_sum(m2) * l2 then true else false
			end
			else false
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
  = fun exp -> (*raise NotImplemented*)
		match exp with
		| X -> raise NotImplemented
		| INT a -> a
		| ADD (a,b) -> calculator a + calculator b
		| SUB (a,b) -> calculator a - calculator b
		| MUL (a,b) -> calculator a * calculator b
		| DIV (a,b) -> calculator a / calculator b
    | SIGMA (f,t,e) ->
			let i = (calculator f) and j = (calculator t) in
			if i>j then 0 else eval (e,f) + calculator(SIGMA(ADD(f,INT 1),t,e))
		
	and eval (f,x) =
		match f with
		| X -> calculator x
		| INT a -> a
		| ADD(f1,f2) -> eval(f1,x) + eval(f2,x)
		| SUB(f1,f2) -> eval(f1,x) - eval(f2,x)
		| MUL(f1,f2) -> eval(f1,x) * eval(f2,x)
		| DIV(f1,f2) -> eval(f1,x) / eval(f2,x)
		| SIGMA (i,j,f) -> calculator(SIGMA(i,j,f))
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


	let rec find : var list * var -> bool
	= fun (lst,v) ->
	 match lst with
	| hd::tl -> if hd=v then true else find(tl,v)
	| _-> false

	let rec f: var list * exp -> bool
	= fun	(lst,e) ->
		match e with
		| V v -> if find(lst, v) then true  else false
		| P (v,e) -> if f(lst@[v],e) then true else false
		| C (e1,e2) -> if check(e1)&&check(e2) then true else false

  and check : exp -> bool
  = fun exp -> (*raise NotImplemented*)
		match exp with
		| V v -> false
		| P (v,e) -> if f([v],e) then true else false
		| C (e1,e2) -> if check(e1)&&check(e2) then true else false
end
