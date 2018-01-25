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

  let diff : aexp * string -> aexp
  = fun (exp, var) -> raise NotImplemented (* TODO *)
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
	
	let torque : (int * int) -> int
	= fun (a,b) -> a*b
	
	let rec comweight : branch -> int
	= fun brc -> match brc with
		| SimpleBranch (l1,w1) -> w1
		| CompoundBranch (l1,(brc1,brc2)) -> comweight brc1 + comweight brc2
	
  let rec balanced : mobile -> bool
  = fun mob -> 
		let (brc1,brc2) = mob in
		match mob with
			| (SimpleBranch (l1,w1), SimpleBranch (l2,w2)) -> if torque (l1,w1) = torque (l2,w2) then true else false
			| (SimpleBranch (l1,w1), CompoundBranch (l2,m2)) -> if balanced m2 && torque (l1,w1) = torque (l2,comweight brc2) then true else false
			| (CompoundBranch (l1,m1), SimpleBranch (l2,w2)) -> if balanced m1 && torque (l2,w2) = torque (l1,comweight brc1) then true else false
			| (CompoundBranch (l1,m1), CompoundBranch (l2,m2)) -> if balanced m1 && balanced m2 && torque (l1,comweight brc1) = torque (l2,comweight brc2) then true else false
		 
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

let rec semi_sigma : exp -> int -> int
	= fun expect n -> match expect with
		| X -> n
		| INT m -> m				
		| ADD (a,b) -> semi_sigma a n + semi_sigma b n
		| SUB (a,b) -> semi_sigma a n - semi_sigma b n
		| MUL (a,b) -> semi_sigma a n * semi_sigma b n
		| DIV (a,b) -> semi_sigma a n / semi_sigma b n

let rec sigma : int -> int -> exp -> int
  = fun n1 n2 expec -> if n1=n2 then (semi_sigma expec n1) else (semi_sigma expec n1) + sigma (n1+1) n2 expec
  	
  let rec calculator : exp -> int
  = fun exp -> match exp with
		| INT n -> n
		| ADD (n1,n2) -> calculator n1 + calculator n2
		| SUB (n1,n2) -> calculator n1 - calculator n2
		| MUL (n1,n2) -> calculator n1 * calculator n2
		| DIV (n1,n2) -> calculator n1 / calculator n2
		| SIGMA (n1,n2,f) -> sigma (calculator n1) (calculator n2) f
	 
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

  let check : exp -> bool
  = fun exp -> raise NotImplemented (* TODO *)
end

