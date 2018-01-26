(*problem1*)

(*module Problem1 = struct
type aexp=
	|Const of int
	|Var of string
	|Power of string*int
	|Times of aexp list
	|Sum of aexp list

let rec diff:aexp *string-> aexp
=fun (exp,var)->
match exp with
|Const a->Const 0
|Var x->if x=var then Const 1 else Const 0
|Power (s,i)->if s=var then Times[Const i;Power(s,i-1)] else Const 0
|Times a_l-> 
	begin match a_l with
		|hd::tl->Sum([Times[diff(hd,var)::tl]]@[Times[tl]])
	end
|Sum a_l->
	begin	match a_l with
		|hd::tl->Sum[diff(hd,var);diff(tl,var)]
	end
end
*)
(*problem2*)
module Problem2 = struct
	type mobile=branch*branch
	and branch=
		|SimpleBranch of length*weight
		|CompoundBranch of length*mobile
	and length=int
	and weight=int
	let rec weight_ :branch->int
	=fun l-> 
	match l with
		|SimpleBranch(l1,w1)->w1
		|CompoundBranch(l1,(b1,b2))-> weight_(b1)+weight_(b2)
	
	let rec balanced : mobile->bool
	=fun mob->
	match mob with
		|SimpleBranch(l1,w1),SimpleBranch(l2,w2)->if (l1*w1=l2*w2) then true else false
		|SimpleBranch(l1,w1),CompoundBranch(l2,m) | CompoundBranch(l2,m),SimpleBranch(l1,w1)-> (balanced (m)) && (l1*w1=l2*(weight_ (CompoundBranch(l2,m))))
		|CompoundBranch(l1,m1), CompoundBranch(l2,m2)-> (balanced (m1)) &&(balanced (m2)) && (l1*(weight_(CompoundBranch(l1,m1)))=l2*(weight_(CompoundBranch(l2,m2))))
end
(*
(*problem3*)
module Problem3 = struct
	type exp=
	|X
	|INT of int
	|ADD of exp*exp
	|SUB of exp*exp
	|MUL of exp*exp
	|DIV of exp*exp
	|SIGMA of exp*exp*exp
let rec calculator: exp->int
=fun exp->
match exp with

|INT a ->a
|ADD(e1,e2)->(calculator(e1))+(calculator(e2))
|SUB(e1,e2)->(calculator(e1))-(calculator(e2))
|MUL(e1,e2)->(calculator(e1))*(calculator(e2))
|DIV(e1,e2)-> (calculator(e1))/(calculator(e2)) 
|SIGMA(e1,e2,e3)->if (calculator(e1)>calculator(e2)) then 0 
									else let X=e1 
										in calculator(ADD(e3,SIGMA(ADD(e1,INT 1),e2,e3)))		
end

*)
(*
(*problem 4*)
module Problem4 = struct
type exp=
|V of var
|P of var*exp
|C of exp*exp
and var=string

let check: exp->bool
=fun exp->*)
