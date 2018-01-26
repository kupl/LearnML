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
  = fun (exp, var) -> match exp with
	| Const(a) -> Const 0
	| Var(x) -> if (x=var) then Const 1 else Const 0
	| Power(s,a) -> if (s=var) then Times[Const a; Power(s,a-1)] else Const 0
	| Times lst -> (match lst with
		| [] -> Const 0
		| hd::tl -> Sum[(Times(diff(hd, var)::tl));(Times[hd; diff(Times tl, var)])]
		)
	| Sum lst -> (match lst with
		| [] -> Const 0
		| hd::tl -> Sum[(diff(hd,var));(diff(Sum tl, var))]
		)
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

  let rec raiseweight : mobile -> int
  = fun mob -> match mob with
              |(SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> lw + rw
              |(CompoundBranch(ll,lw), SimpleBranch(rl,rw)) -> raiseweight(lw) + rw
              |(SimpleBranch(ll,lw), CompoundBranch(rl,rw)) -> lw + raiseweight(rw)
              |(CompoundBranch(ll,lw), CompoundBranch(rl,rw)) -> raiseweight(lw) + raiseweight(rw)

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
							|(SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> if (ll*lw)=(rl*rw) then true else false
							|(CompoundBranch(ll,lw), SimpleBranch(rl,rw)) -> if (((ll*(raiseweight(lw)))=(rl*rw))&&(balanced(lw))) then true else false 
							|(SimpleBranch(ll,lw), CompoundBranch(rl,rw)) -> if (((ll*lw)=(rl*raiseweight(rw)))&&balanced(rw)) then true else false 
							|(CompoundBranch(ll,lw), CompoundBranch(rl,rw)) -> if (((ll*raiseweight(lw))=(rl*raiseweight(rw)))&&balanced(lw)&&balanced(rw)) then true else false
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
  let rec checkx : (exp*exp) -> exp
  = fun (a,b) -> match a with
    |X -> b
    |INT(x) -> INT(x)
    |ADD(x,y) -> ADD(checkx(x,b), checkx(y,b))
    |SUB(x,y) -> SUB(checkx(x,b), checkx(y,b))
    |MUL(x,y) -> MUL(checkx(x,b), checkx(y,b))
    |DIV(x,y) -> DIV(checkx(x,b), checkx(y,b))
    |SIGMA(x,y,z) -> SIGMA(checkx(x,b),checkx(y,b),z)

  let rec calculator : exp -> int
  = fun exp -> match exp with
		|X -> raise NotImplemented
		|INT(a) ->  a 
		|ADD(a,b) -> calculator(a)+calculator(b)
		|SUB(a,b) -> calculator(a)-calculator(b)
		|MUL(a,b) -> calculator(a)*calculator(b)
		|DIV(a,b) -> calculator(a)/calculator(b)
		|SIGMA(first, last, yap) 
		-> if (calculator(first)=calculator(last)) 
				then (calculator(checkx(yap,first)))  else (calculator(SIGMA(INT(calculator(first)+1),last,yap))+calculator(checkx(yap,first))) 
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

	let rec para : (exp*string list) -> bool
	= fun (exp, lst) -> match exp with
	| V(x) -> (let rec ac : (string list*string) -> bool
						=fun (oh, ho) -> (match oh with
						|[]-> false
						|hd::tl-> if(hd=ho) then true else (ac(tl, ho))
						)
						in ac(lst, x))   
	| P(x,e) -> para(e, x::lst)
	| C(e1, e2) -> ((para(e1, lst))&&(para(e2, lst)))


  let rec check : exp -> bool
  = fun exp -> para(exp, [])

end

