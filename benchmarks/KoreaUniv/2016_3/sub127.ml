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
  		| Const(a) -> Const(0)
  		| Var(a) -> if a = var then Const(1)
  					else Const(0)
  		| Power(a,b) -> if  a <> var then Const(0)
  						else if b = 1 then Const(1)
  						else Times[Const(b);Power(a,b-1)]
  		| Times(a) -> 
  			begin
  			match a with 
  				| []->Const(0)
  				| hd::tl -> Sum[Times(diff(hd,var)::tl);Times[hd;diff(Times(tl),var)]]
  			end
  		| Sum(a) ->
  			begin 
  			match a with 
  				| []->Const(0)
  				| hd::tl -> Sum[diff(hd,var);diff(Sum(tl),var)]
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

  let rec f : mobile->int
  = fun mob -> 
  	match mob with 
  		| (SimpleBranch(a,b),SimpleBranch(c,d)) -> (b+d)
  		| (SimpleBranch(a,b), CompoundBranch(c,d)) -> (b+(f d))
  		| (CompoundBranch(a,b),SimpleBranch(c,d)) -> ((f b)+d)
  		| (CompoundBranch(a,b),CompoundBranch(c,d)) -> ((f b)+(f d))

  let rec balanced : mobile -> bool
  = fun mob -> 
  	match mob with 
  		| (SimpleBranch(a,b),SimpleBranch(c,d)) -> (a*b) = (c*d)
  		| (SimpleBranch(a,b), CompoundBranch(c,d)) ->((a*b) = (c*(f d)))&&(balanced d)
  		| (CompoundBranch(a,b),SimpleBranch(c,d)) -> ((a*(f b)) = (c*d))&&(balanced b)
  		| (CompoundBranch(a,b),CompoundBranch(c,d)) ->((a*(f b)) = (c*(f d)))&&(balanced b)&&(balanced d)
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

let rec change: exp -> int -> exp
  = fun exp a->
  	match exp with
  		| X -> INT a
  		| INT(b) ->INT(b)
  		| ADD(b,c) -> ADD((change b a), (change c a))
  		| SUB(b,c) ->SUB((change b a), (change c a))
  		| MUL(b,c) -> MUL((change b a), (change c a))
  		| DIV(b,c) -> DIV((change b a), (change c a))
  		| SIGMA(b,c,d) -> SIGMA((change b a), (change c a), d)


  let rec calculator : exp -> int
  = fun exp -> 
  	match exp with 
  		| X -> raise NotImplemented
  		| INT(a) -> a
  		| ADD(a,b) -> calculator(a) + calculator(b)
  		| SUB(a,b) -> calculator(a) - calculator(b)
  		| MUL(a,b) -> calculator(a) * calculator(b)
  		| DIV(a,b) -> calculator(a) / calculator(b)
  		| SIGMA(a,b,c) -> if (calculator a > calculator b) then 0 
  						  else calculator(change c (calculator a)) + calculator(SIGMA(ADD(a,INT(1)),b,c))

  
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

  let rec matchi : exp -> string list -> bool
  = fun exp lst -> 
  	match exp with
  	| V(a) -> 
  		begin
  		match lst with 
  			|[] -> false
  			|hd::tl ->if(hd = a) then true
  					 else matchi exp tl
  		end
  	| P(a,b) -> matchi b ([a]@lst)
  	| C(a,b) -> (matchi a lst)&&(matchi b lst)


  let rec check : exp -> bool
  = fun exp ->  
  	match exp with
  		| V(a) -> matchi exp []
  		| P(a,b) -> matchi b [a]
  		| C(a,b) -> (matchi a [])&&(matchi b [])
end

