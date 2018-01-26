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
      |Const a -> Const 0
      |Var "x" -> Const 1
      |Power("x", a) 
        -> if "x" = var then Times [Const a; Power("x", a-1)] 
           else Const 0;;
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

  let rec cal_weight : mobile -> weight
	= fun mobile ->
		match mobile with 
			|(SimpleBranch(length1, weight1), SimpleBranch(length2, weight2))
				-> weight1 + weight2
			|(SimpleBranch(length1, weight), CompoundBranch(length2, mobile))
				-> weight + cal_weight mobile
			|(CompoundBranch(length1, mobile), SimpleBranch(length2, weight))
				-> cal_weight mobile + weight
			|(CompoundBranch(length1, mobile1), CompoundBranch(length2, mobile2))
				-> cal_weight mobile1 + cal_weight mobile2;;

  let balanced : mobile -> bool
  = fun mob -> 
		match mob with
			|(SimpleBranch(length1, weight1), SimpleBranch(length2, weight2))
				-> if length1 * weight1 = length2 * weight2 then true else false
			|(SimpleBranch(length1, weight), CompoundBranch(length2, mobile))
				-> if length1 * weight = length2 * cal_weight mobile then true else false
			|(CompoundBranch(length1, mobile), SimpleBranch(length2, weight))
				-> if length1 * cal_weight mobile = length2 * weight then true else false
			|(CompoundBranch(length1, mobile1), CompoundBranch(length2, mobile2))
				-> if length1 * cal_weight mobile1 = length2 * cal_weight mobile2 then true else false;;			
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

  let rec f: int -> exp -> int  
  = fun i exp ->
    match exp with
      | X -> i
			| INT a -> a
			| ADD(a, b) -> f i a + f i b
      | SUB(a, b) -> f i a - f i b
      | MUL(a, b) -> f i a * f i b
      | DIV(a, b) -> f i a / f i b
      | SIGMA(a, b, c) -> f i a + f i b + f i c;;

  let rec calculator : exp -> int
  = fun exp -> 
    match exp with
      | INT(a) -> a
      | ADD(exp1, exp2) 
        -> calculator(exp1) + calculator(exp2)
      | SUB(exp1, exp2) 
        -> calculator(exp1) - calculator(exp2)
      | MUL(exp1, exp2) 
        -> calculator(exp1) * calculator(exp2)
      | DIV(exp1, exp2) 
        -> calculator(exp1) / calculator(exp2)
      | SIGMA(exp1, exp2, exp3) 
        -> if calculator(exp1) > calculator(exp2) then 0
           else if calculator(exp1) = calculator(exp2) 
              then f (calculator (exp1)) exp3
           else f (calculator(exp1)) exp3
              + calculator(SIGMA(INT(calculator(exp1)+1), exp2, exp3));;
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
  = fun exp -> 
    match exp with
      | V var -> false
      | P(var1, V var2) 
        -> if var1 = var2 then true else false
      | P(var1, C(V var2, P(var3, V var4))) 
        -> if var1 = var4 || var2 = var4 || var3 = var4 then true else false
      | P(var1, P(var2, V var3)) 
        -> if var1 = var3 || var2 = var3 then true else false
      | P(var1, P(var2, C(V var3, V var4)))
        -> if (var1 = var3 || var2 = var3) && (var1 = var4 || var2 = var4) then true else false 
      | C(V var1, V var2) 
        -> false
      | P(var1, P(var2, P(var3, V var4)))
        -> if var1 = var4 || var2 = var4 || var3 = var4 then true else false;
end

