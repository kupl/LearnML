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
  |  Sum(a) -> (
 	match a with
	|hd::tl -> (Sum((diff (hd,var))::[diff (Sum(tl),var)]))
	|_ -> Const(0) )
  | Times(a) ->(
	match a with
	 hd::tl -> Sum(Times((diff (hd,var))::tl)::[Times(hd::([diff (Times(tl),var)]))])
	|_ -> Const(0))
  | Power(a,b)-> if(a=var&&b>1) then Times(Power(a,b-1)::[Const(b)])
		 else if(b=1) then Const(1)
		 else Const(0)
  | Var(a) -> if(a=var) then Const(1) else Const(0)
  | Const(a)-> Const(0)
	
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
  l,r -> match l,r with
	  SimpleBranch(a,b),SimpleBranch(c,d) ->b+d 
	 |SimpleBranch(a,b),CompoundBranch(c,d) -> b + (sum d)
	 |CompoundBranch(a,b),SimpleBranch(c,d) -> (sum b) + d
	 |CompoundBranch(a,b),CompoundBranch(c,d) -> (sum b) + (sum d)

  
  let rec balanced : mobile -> bool
  = fun mob ->
  match mob with 
  l,r -> match l,r with
	 SimpleBranch(a,b),SimpleBranch(c,d) -> if(a*b=c*d) then true else false
	|SimpleBranch(a,b),CompoundBranch(c,d) ->
						 if((balanced d)&&(sum d)*c=a*b)
						  then true else false
	|CompoundBranch(a,b),SimpleBranch(c,d) ->
						 if((balanced b)&&(sum b)*a=c*d)
						  then true else false
	|CompoundBranch(a,b),CompoundBranch(c,d) ->
		 if((balanced b)&&(balanced d)&&(sum b)*a=(sum d)*c) then true 
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
  
  let rec calX
  = fun i exp ->
  match exp with
  X -> i
 |INT(a) -> a
 |ADD(a,b) -> (calX i a) + (calX i b)
 |MUL(a,b) -> (calX i a) * (calX i b)
 |SUB(a,b) -> (calX i a) - (calX i b)
 |DIV(a,b) -> if((calX i b)=0) then raise NotImplemented else (calX i a)/(calX i b) 
 |SIGMA(a,b,c) -> if((calX i a)<=(calX i b)) then (calX i c) + (calX i (SIGMA(INT((calX i a)+1),b,c))) else 0 
 
 let rec calculator : exp -> int
  = fun exp ->
  match exp with
  X -> raise NotImplemented
 |INT(a) -> a
 |ADD(a,b) -> (calculator a)+(calculator b)
 |SUB(a,b) -> (calculator a)-(calculator b)
 |MUL(a,b) -> (calculator a)*(calculator b)
 |DIV(a,b) -> if((calculator b)=0) then raise NotImplemented else (calculator a)/(calculator b)	      
 |SIGMA(a,b,c) -> if((calculator a)<=(calculator b)) then (calX (calculator a) c) + calculator(SIGMA(INT((calculator a)+1),b,c)) else 0
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

  let rec chk
  = fun a var->
  match a with
  |hd::tl -> if(var=hd) then true else (chk tl var)
  |_ -> false   
 
  let lst = []    

  let rec chk2
  = fun lst exp ->
  match exp with
  | P(a,b) -> chk2 (a::lst) b
  | C(a,b) -> if((chk2 lst a)&&(chk2 lst b)) then true else false
  | V(a) -> if(chk lst a) then true else false


  let rec check : exp -> bool
  = fun exp -> chk2 [] exp

end

