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

	let rec contain : aexp list * string -> bool 
	= fun (exp, var) -> match (exp, var) with 
	  | ([],x) -> false
	  | (Const a) :: tl, x -> contain (tl, x)
	  | (Var a) :: tl, x -> if a = x then true else contain (tl, x)
	  | (Power(x,a)) :: tl, y -> if x = y && a!=0 then true else contain (tl, x)
	  | _,_ -> raise (Failure "Invalid input")
	  
	let rec timesHelper : aexp list * string -> aexp list 
	= fun (exp, var) -> match (exp, var) with
	  | ([],x) -> []
	  | (Const a) :: tl, x -> Const a :: (timesHelper (tl, x))
	  | (Var a) :: tl, x -> if a = x then Const (1) :: (timesHelper (tl, x)) else Var a :: (timesHelper (tl, x))
	  | (Power(x,a)) :: tl, y -> if x = y && a !=0 then Times [Const (a); Power (x,(a-1))] :: (timesHelper (tl, y)) else (Power( x, a)) :: (timesHelper (tl, y))
	  | _,_ -> raise (Failure "Invalid input")
		
	let rec sumHelper : aexp list * string -> aexp list 
	= fun (exp, var) -> match (exp, var) with
	  | ([],x) -> []
	  | (Const a) :: tl, x -> Const (0) :: sumHelper (tl, x) 
	  | (Var a) :: tl, x -> if a = x then Const (1) :: (sumHelper (tl, x)) else sumHelper(tl, x)
	  | (Power(x,a)) :: tl, y -> if x = y && a !=0 then Times [Const (a); Power (x, (a-1))] :: (sumHelper (tl, y)) else Const (0) :: sumHelper (tl, y)
	  | (Times (t)) :: tl, x -> if contain (t, x) then Times (timesHelper (t, x)) :: (sumHelper (tl, x)) else Const (0) :: (sumHelper (tl, x))
	  | _,_ -> raise (Failure "Invalid input")
	  
	let rec diff : aexp * string -> aexp
	= fun (exp, var) -> match (exp, var) with 
	  | (Const a, x)  -> Const (0)
	  | (Var a, x) -> if a = x then Const (1) else Const (0)
	  | (Power(x, a), y) -> if x = y && a !=0 then Times [Const (a); Power (x, (a-1))] else Const (0)
	  | (Times (t), x) -> if contain (t, x) then Times (timesHelper (t, x)) else Const (0)
	  | (Sum (s), x) -> Sum (sumHelper (s, x))
  
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
  
	  let rec getWeight : mobile -> int 
	  = fun mob -> match mob with
		| (SimpleBranch(leftLength,leftWeight), SimpleBranch(rightLength,rightWeight)) -> leftWeight + rightWeight
		| (SimpleBranch(leftLength,leftWeight), CompoundBranch (rightLength, mob1)) -> leftWeight + getWeight (mob1)
		| (CompoundBranch(leftLength, mob2), SimpleBranch (rightLength,rightWeight)) -> getWeight(mob2) + rightWeight
		| (CompoundBranch(leftLength, mob3), CompoundBranch (rightLength, mob4)) -> getWeight (mob3) + getWeight (mob4)

	  let rec balanced : mobile -> bool
	  = fun mob -> match mob with
		| (SimpleBranch(leftLength,leftWeight), SimpleBranch(rightLength,rightWeight)) -> if leftLength*leftWeight = rightLength*rightWeight then true else false 
		| (SimpleBranch(leftLength,leftWeight), CompoundBranch (rightLength, mob1)) -> if balanced(mob1) && leftLength*leftWeight = rightLength*getWeight(mob1) then true else false
		| (CompoundBranch(leftLength, mob2), SimpleBranch (rightLength,rightWeight)) -> if balanced(mob2) && leftLength*getWeight(mob2) = rightLength*rightWeight then true else false
		| (CompoundBranch(leftLength, mob3), CompoundBranch (rightLength, mob4)) -> if balanced(mob3) && balanced(mob4) && leftLength*getWeight(mob3) = rightLength*getWeight(mob4) then true else false
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
  
	  let rec sigmaCalc: exp * int -> int 
	  = fun (exp, a) -> match (exp, a) with 
		| X, a -> a
		| INT x,a -> x
		| ADD (x, y),a -> sigmaCalc (x,a) + sigmaCalc (y,a)
		| SUB (x, y),a -> sigmaCalc (x,a) - sigmaCalc (y,a)
		| MUL (x, y),a -> sigmaCalc (x,a) * sigmaCalc (y,a)
		| DIV (x, y),a -> sigmaCalc (x,a) / sigmaCalc (y,a)
		| SIGMA (INT (x), INT (y), z),a -> if x > y then raise (Failure "Invalid input") else if x = y then sigmaCalc (z,a) else sigmaCalc (z,a) + sigmaCalc(SIGMA(INT(x+1), INT (y), z), (x+1)) 
		| SIGMA (x, y, z),a -> raise (Failure "Invalid input")

	  let rec calculator : exp -> int
	  = fun exp -> match exp with 
		| X -> raise (Failure "Invalid input")
		| INT x -> x
		| ADD (x, y) -> calculator x + calculator y
		| SUB (x, y) -> calculator x - calculator y
		| MUL (x, y) -> calculator x * calculator y
		| DIV (x, y) -> calculator x / calculator y
		| SIGMA (INT (x), INT (y),z) -> if x > y then raise (Failure "Invalid input") else sigmaCalc (exp, x)
		| SIGMA (x, y, z) -> raise (Failure "The first two varibles of Sigma exp have to be integers")
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
	  
	  let rec bound : exp -> var list 
	  = fun exp -> match exp with 
	  | V var -> []
	  | P (var, ex) -> var :: bound(ex)
	  | C (ex1, ex2) -> bound(ex1)@bound(ex2)
	  
	  let rec variables: exp -> var list
	  = fun exp -> match exp with 
	  | V var -> [var]
	  | P (var, ex) -> variables (ex)
	  | C (ex1, ex2) -> variables (ex1)@variables(ex2)
	  
	  let rec containHelper : var list * var -> bool 
	  = fun (bound, a) -> match (bound, a) with 
	  | ([], a) -> false
	  | (x :: tl, a) -> if x = a then true else containHelper(tl, a)
	  
	  let rec contain : var list * var list -> bool
	  = fun (bound, variables) -> match (bound, variables) with
	  | ([], variables) -> false
	  | (bound, []) -> true
	  | (bound, a::tl) -> if containHelper(bound, a) = false then false else contain (bound, tl)

	  let check : exp -> bool
	  = fun exp -> contain(bound(exp), variables(exp))
end

