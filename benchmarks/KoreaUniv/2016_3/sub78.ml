(*mail test*)
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

  let rec length = fun lst ->
  match lst with
  | [] -> 0
  | hd::tl -> 1 + length tl

  let hd = fun lst -> 
  match lst with
  | hd::tl -> hd

  let tl = fun lst ->
  match lst with
  | hd::tl -> tl

    let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
  | Power(var1, int1) -> if(var1 = var) then (if (int1 == 2) then Times[Const int1; Var var1] 
                                                 else Times[Const int1; Power(var1, int1 - 1)])
                           else Const 0
  | Const (int1) -> Const 0
  | Var (var1) -> if(var1 = var) then Const 1 else Const 0
  | Sum(lst) -> let rec protoSum = fun tolst fromlst var ->
  match fromlst with
    | [] -> Sum(tolst)
    | hd::tl -> protoSum (tolst@[diff(hd, var)]) tl var
  in protoSum [] lst var

  | Times(lst) -> let rec protoTime = fun tolst fromlst var pivot ->
    if(pivot = 0) then Sum(tolst) 
    else protoTime (tolst@[Times(diff (hd fromlst, var)::(tl fromlst))]) 
                      ((tl fromlst) @ [hd fromlst]) var (pivot - 1)
  in protoTime [] lst var (length lst)
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

  let rec getWeight
	= fun bran -> match bran with
	SimpleBranch(length, weight) -> weight
|	CompoundBranch(length, mob) -> match mob with
	(left, right) -> getWeight(left) + getWeight(right)

let getLength
	= fun bran -> match bran with
	SimpleBranch(length, weight) -> length
|	CompoundBranch(length, mob) -> length

  let rec balanced : mobile -> bool
  = fun mob -> match mob with
		(left, right) ->
		if(getWeight(left) * getLength(left) == getWeight(right) * getLength(right))
		 then match (left, right) with

			(SimpleBranch(len1, wei1), SimpleBranch(len2, wei2)) ->
			true
		| (SimpleBranch(len1, wei1), CompoundBranch(len2, mob2)) ->
			if((balanced mob2) == true) then true else false
		| (CompoundBranch(len1, mob1), SimpleBranch(len2, wei2)) ->
			if((balanced mob1) == true) then true else false
		| (CompoundBranch(len1, mob1), CompoundBranch(len2, mob2)) ->
			if((balanced mob1) == true && (balanced mob2) == true) 
			then true else false
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

  let rec subsum : exp -> exp -> int
  = fun vals exps -> match exps with
    | INT k -> k
    | X -> subsum vals vals
    | ADD (exp1, exp2) -> (subsum vals exp1) + (subsum vals exp2)
    | SUB (exp1, exp2) -> (subsum vals exp1) - (subsum vals exp2)
    | MUL (exp1, exp2) -> (subsum vals exp1) * (subsum vals exp2)
    | DIV (exp1, exp2) -> (subsum vals exp1) / (subsum vals exp2)
    | SIGMA (expStart, expEnd, exp1) -> if(subsum vals expStart <= subsum vals expEnd)
      then (subsum expStart exp1) + (subsum vals (SIGMA(ADD(expStart, INT 1), expEnd, exp1))) else 0

  let rec calculator : exp -> int
  = fun exp -> match exp with
    | INT k -> k
    | ADD (exp1, exp2) -> (calculator exp1) + (calculator exp2)
    | SUB (exp1, exp2) -> (calculator exp1) - (calculator exp2)
    | MUL (exp1, exp2) -> (calculator exp1) * (calculator exp2)
    | DIV (exp1, exp2) -> (calculator exp1) / (calculator exp2)
    | SIGMA (expStart, expEnd, exp1) -> if(calculator expStart <= calculator expEnd)
      then (subsum expStart exp1) + (calculator (SIGMA(ADD(expStart, INT 1), expEnd, exp1))) else 0
    | _ -> 0

   
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

 

  let rec checklst (*true if there is no this value before*)
  = fun lst val1 -> match lst with
  | hd::tl -> if((compare hd val1) == 0) then false else checklst tl val1
  | [] -> true

  let rec checkExp
  = fun lst exp -> match exp with
  | V var -> if checklst lst var then false else true
  | P (var, exp) -> if checklst lst var then checkExp (var::lst) exp else checkExp lst exp
  | C (exp1, exp2) ->if (checkExp lst exp1 == true && checkExp lst exp2 == true) then true else false

   let check : exp -> bool
  = fun exp -> let varlst = [] in checkExp varlst exp
end
