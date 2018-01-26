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
  
let rec calcPower : aexp * string -> aexp
	= fun (power, var) ->
		match power, var with 
		|Power (variable, exponent), diffVar -> 
		if variable = diffVar then Times [Const(exponent); Power(variable, (exponent-1))] 
		else Power(variable, exponent)
		|_, _ -> raise (Failure "wrong input for Power Calculation")
	
let rec contains : aexp list * string -> bool
	= fun (l, var) -> 
		match l, var with
		|hd::tl, diffVar -> if hd = Var(diffVar) then true else if tl = []	 then false else contains (tl, diffVar)
		|_,_ -> raise (Failure "wrong input for contains check")
		
let rec noZero : aexp list -> bool
	= fun l -> 
		match l with
		|hd::tl -> if hd = Const(0) then false else if tl = [] then true else noZero (tl)
		|_-> raise (Failure "wrong input for zero check")
		
let rec calcTimes : aexp list * string -> aexp list
	= fun (l, var) -> 
		match l, var with
		|[], diffVar -> []
		|Const(i)::tl, diffVar -> Const(i):: calcTimes(tl, diffVar)
		|Var (s):: tl, diffVar -> if s = diffVar then calcTimes(tl, diffVar) else Var(s):: calcTimes(tl, diffVar)
		|Power(x, exponent)::tl, diffVar -> calcPower(Power(x, exponent), diffVar):: calcTimes(tl, diffVar)
		|_,_ -> raise (Failure "wrong input for multiplication")
		
	let rec calcSum : aexp list * string -> aexp list
	= fun (l, var) -> 
		match l, var with
		|[], diffVar -> []
		|(Const i)::tl, diffVar -> if tl = [] then [Const(0)] else calcSum(tl, diffVar)
		|(Var s)::tl, diffVar -> if s = diffVar then [Const(1)] else calcSum(tl, diffVar)
		|Power(variable, exponent)::tl, diffVar -> if variable = diffVar then calcPower(Power(variable, exponent), diffVar):: calcSum(tl, diffVar) else calcSum(tl, diffVar)
		|Times(x)::tl, diffVar -> 
		if contains(x, diffVar) && noZero(l) then (calcTimes(x, diffVar)@ calcSum(tl, diffVar))
		else calcSum(tl, diffVar)
		| _, _-> raise (Failure "wrong input for sum")
		
		
let diff : aexp * string -> aexp
  = fun (exp, var) -> 
	match exp, var with 
	|Const c, diffVar -> Const(0)
	|Var s, diffVar -> if s = diffVar then Const(1) else Const(0)
	|Power(variable, exponent), diffVar -> if variable = diffVar then calcPower(Power(variable, exponent), diffVar) else Const(0)
	|Times (l), diffVar -> if contains(l, diffVar) && noZero(l) then Times(calcTimes (l, diffVar))
	else Const(0)
	|Sum(l), diffVar -> Sum(calcSum (l, diffVar))		

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
	= fun mob ->
		match mob with 
		|(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> w1+w2
		|(CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> getWeight(m1)+w2
		|(SimpleBranch(l1, w1), CompoundBranch(l2, m2))-> w1+getWeight(m2)
		|(CompoundBranch(l1, m1), CompoundBranch(l2, m2))-> getWeight(m1) + getWeight(m2)

  let rec balanced : mobile -> bool
  = fun mob -> 
	match mob with
		|(SimpleBranch(l1, w1), SimpleBranch(l2, w2)) -> if (l1*w1)= (l2*w2) then true else false
		|(CompoundBranch(l1, m1), SimpleBranch(l2, w2)) -> if (l1*getWeight(m1)) = (l2*w2) && balanced m1 then true else false
		|(SimpleBranch(l1, w1), CompoundBranch(l2, m2))-> if (l2*getWeight(m2)) = (l1*w1) && balanced m2 then true else false
		|(CompoundBranch(l1, m1), CompoundBranch(l2, m2))-> if (l1*getWeight(m1))= (l2*getWeight(m2)) && balanced m1 && balanced m2 then true else false
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
  
let rec calculatorHelper : exp * int -> int 
 = fun (exp, xValue) ->
	match exp, xValue with
	|X, xValue -> xValue
	|INT i, xValue -> i
	|ADD (exp1, exp2), xValue -> calculatorHelper (exp1, xValue) + calculatorHelper (exp2, xValue)
	|SUB (exp1, exp2), xValue -> calculatorHelper (exp1, xValue) - calculatorHelper (exp2, xValue)
	|MUL (exp1, exp2), xValue -> calculatorHelper (exp1, xValue) * calculatorHelper (exp2, xValue)
	|DIV (exp1, exp2), xValue -> calculatorHelper (exp1, xValue) / calculatorHelper (exp2, xValue)
	|SIGMA (INT(a), INT (b), exp ), xValue -> 
			if a > b then raise (Failure "First Value of Sigma must be 		smaller or equal to the second value")
			else if a = b then calculatorHelper(exp, a)
			else calculatorHelper(exp, a) + calculatorHelper((SIGMA (INT(a+1), INT (b), exp)), xValue)
			
		
 
 let rec calculator : exp -> int 
 = fun exp ->
	match exp with
	|X -> raise (Failure "X is not defined")
	|INT i -> i
	|ADD (exp1, exp2) -> calculator exp1 + calculator exp2
	|SUB (exp1, exp2) -> calculator exp1 - calculator exp2
	|MUL (exp1, exp2) -> calculator exp1 * calculator exp2
	|DIV (exp1, exp2) -> calculator exp1 / calculator exp2
	|SIGMA (INT(a), INT (b), exp ) ->
			if a > b then raise (Failure "First Value of Sigma must be 		smaller or equal to the second value")
			else if a = b then calculatorHelper(exp, a)
			else calculatorHelper(exp, a) + calculator(SIGMA (INT(a+1), INT (b), exp))
		
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
  
let rec getBoundValues : exp -> var list
	= fun exp ->
	match exp with
	| V v -> []
	| P (v, e) -> v::getBoundValues(e)
	| C (e1, e2) -> getBoundValues(e1)@getBoundValues(e2)
	
let rec getValues : exp -> var list
	= fun exp ->
	match exp with
	|V v -> [v]
	|P (v, e) -> getValues(e)
	|C (e1, e2) -> getValues(e1)@getValues(e2)
	
let rec compareHelper : var * var list -> bool
	= fun (var, varL) ->
	match var, varL with
	|v, hd::tl -> if v = hd then true else if tl = [] then false else compareHelper(v, tl)
	
let rec compare : var list * var list-> bool
	= fun (values, bound) -> 
	match values, bound with
	| valueHd::valueTl, bound ->  
	if valueTl = [] then compareHelper(valueHd, bound) 
	else if compareHelper(valueHd, bound) then compare(valueTl, bound)
	else false
	
  let check : exp -> bool
  = fun exp -> compare(getValues(exp), getBoundValues(exp))
end

