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
  = fun (exp, var) -> (* raise NotImplemented  TODO *)
	match exp with
	| Const(n) -> Const(0)
	| Var(str) -> if str=var then Const(1) else Const(0)
	| Power(str,num) ->
		if str = var then(
		if num=0 then Const(0)
		else if num=1 then Const(1)
		else Times([Const num;Power(str,(num-1))])
		)
		else Const(0)
	| Times(lst) ->(
		match lst with
		|[] -> Times([])
		|hd::tl ->
			if tl = [] then diff(hd,var)
			else Sum (Times(diff(hd,var)::tl)::[Times(hd::[diff(Times(tl),var)])])
		)
	| Sum (lst) ->
		match lst with
		|[] -> Sum([])
		|hd::tl -> Sum (diff(hd,var)::[diff(Sum(tl),var)])
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

	let rec calc_w : mobile -> int
	= fun mob ->
	match mob with
	| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> w1+w2
	| (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> w1+calc_w(m2)
	| (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) -> calc_w(m1)+w2
	| (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) -> calc_w(m1)+calc_w(m2)

  let rec balanced : mobile -> bool
  = fun mob -> (* TODO *)
	match mob with
	| (SimpleBranch(l1,w1),SimpleBranch(l2,w2)) -> if l1*w1=l2*w2 then true else false
	| (SimpleBranch(l1,w1),CompoundBranch(l2,m2)) -> 
		if balanced(m2)=false then false 
		else if l1*w1=l2*calc_w(m2) then true else false
	| (CompoundBranch(l1,m1),SimpleBranch(l2,w2)) ->
		if balanced(m1)=false then false
		else if l1*calc_w(m1)=l2*w2 then true else false
	| (CompoundBranch(l1,m1),CompoundBranch(l2,m2)) ->
		if balanced(m1)=false || balanced(m2)=false then false
		else if l1*calc_w(m1)=l2*calc_w(m2) then true else false

end

(*********************)
(*     Problem 3     *)
(*********************)
exception IllegalInput
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

	let rec change : exp * int -> int
	= fun (e,num) ->
	match e with
	| X -> num
	| INT (n) -> n
	| ADD(e1,e2) -> change(e1,num)+change(e2,num)
	| SUB(e1,e2) -> change(e1,num)-change(e2,num)
	| MUL(e1,e2) -> change(e1,num)*change(e2,num)
	| DIV(e1,e2) -> change(e1,num)/change(e2,num)
	| SIGMA(e1,e2,e3) ->
		if change(e1,num)>change(e2,num) then raise IllegalInput
		else if change(e1,num)=change(e2,num) then change(e3,change(e1,num))
		else change(e3,change(e1,num))+change(SIGMA(INT (change(e1,num)+1), INT (change(e2,num)), e3),num)

	and calculator : exp -> int
  = fun exp -> (* TODO *)
	match exp with
	| X -> raise IllegalInput
	| INT(n) -> n
	| ADD(e1,e2) -> calculator(e1)+calculator(e2)
	| SUB(e1,e2) -> calculator(e1)-calculator(e2)
	| MUL(e1,e2) -> calculator(e1)*calculator(e2)
	| DIV(e1,e2) -> calculator(e1)/calculator(e2)
	| SIGMA(e1,e2,e3) ->
		if calculator(e1)>calculator(e2) then raise IllegalInput
		else if calculator(e1)=calculator(e2) then change(e3,calculator(e1))
		else change(e3,calculator(e1))+calculator(SIGMA(INT (calculator(e1) +1), INT (calculator(e2)), e3))
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

	let rec l_match : string * string list -> bool
	= fun(v,vlist) ->
	match vlist with
	| [] -> false
	| hd::tl -> if hd=v then true else l_match(v,tl)

	let rec v_match : exp * string list -> bool
	= fun(exp,vlist) ->
	match exp with
	| V(v) -> l_match(v,vlist)
	| P(v,e) -> v_match(e,v::vlist)
	| C(e1,e2) -> if v_match(e1,vlist)=true && v_match(e2,vlist)=true then true else false

  let rec check : exp -> bool
  = fun exp -> (* raise NotImplemented  TODO *)
	match exp with
	| V(v) -> false
	| P(v,e) -> v_match(e,[v])
	| C(e1,e2) -> if v_match(e1,[])=true && v_match(e2,[])=true then true else false
end

