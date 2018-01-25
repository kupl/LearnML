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
	| Const _ -> Const 0
	| Var str -> if (var = str) then Const 1 else Const 0
	| Power (str,num) -> 
		if (str=var) then
			begin
			match num with
			| 0 -> Const 0
			| _ -> Times [Const num; Power (str, num-1)]
			end
		else Const 0
	| Times [] -> Const 0
	| Times (h::t) -> Sum [Times[h;diff(Times t,var)(*This is aexp!!*)];Times (diff(h,var)::t)] (* VERY hard!!*)
	| Sum exp -> Sum ( List.map(fun x -> diff(x,var)) exp)
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

  let rec weight : mobile -> int 
  = fun mob -> match mob with 
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> w1 + w2
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> w1 + weight (m2)
  | (CompoundBranch (l1, m1), SimpleBranch(l2, w2)) -> weight (m1) + w2
  | (CompoundBranch (l1, m1), CompoundBranch(l2, m2)) -> weight (m1) + weight (m2)
  
  let rec balanced : mobile -> bool
  = fun mob -> match mob with
  | (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if (l1*w1=l2*w2) then true else false
  | (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> if (l1*w1=l2*(weight (m2)) && balanced(m2)) then true else false
  | (CompoundBranch (l1, m1), SimpleBranch(l2, w2)) -> if(l1*(weight (m1)) = l2*w2 && balanced(m1)) then true else false
  | (CompoundBranch (l1, m1), CompoundBranch(l2, m2)) -> if(l1*(weight (m1))=l2*(weight (m2)) && balanced(m1) && balanced(m2)) then true else false
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
(*
  let rec eval exp num = match exp with
  | X -> num
  | INT n -> n
  | ADD (e1,e2) -> (eval e1 num) + (eval e2 num)
  | SUB (e1,e2) -> (eval e1 num) - (eval e2 num)
  | MUL (e1,e2) -> (eval e1 num) * (eval e2 num)
  | DIV (e1,e2) -> (eval e1 num) / (eval e2 num)
  | SIGMA (e1,e2,e3) -> 
    if (e1)=(e2) then (eval e3 (INT e2))
	else (eval e3 e2)+(eval SIGMA(e1,e2-1,e3) 0)
*)
  let rec calculator : exp -> int
  = fun exp -> match exp with 
  | X -> 0
  | INT n -> n
  | ADD (e1,e2) -> (calculator e1) + (calculator e2)
  | SUB (e1,e2) -> (calculator e1) - (calculator e2)
  | MUL (e1,e2) -> (calculator e1) * (calculator e2)
  | DIV (e1,e2) -> (calculator e1) / (calculator e2)
  | SIGMA (e1,e2,e3) -> 0 


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

  let rec chklst (exp,lst) =
  match exp with (* should be call by value *)
  | P(var,exp) -> chklst(exp, lst@[var])
  | C(exp1,exp2) -> if(chklst(exp1, lst) && chklst(exp2, lst)) then true else false
  | V var -> if lst=[] then false
    else List.exists (fun x -> (x=var)) lst

  let check : exp -> bool
  = fun exp -> chklst (exp,[] )
end

