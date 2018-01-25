(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)

module Problem1 = struct
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec weight: int -> bool = fun(a) -> match a with
    | 1 -> true
    | _ -> false

let rec total_weight : mobile -> int =fun (lb,rb) -> match lb, rb with 
		| SimpleBranch(a1, b1), SimpleBranch(a2, b2) -> b1 + b2
		| SimpleBranch(a1, b1), CompoundBranch(c1, (d1, e1)) -> b1 + total_weight(d1, e1)
		| CompoundBranch(a1, (b1, c1)), SimpleBranch(d1, e1) -> total_weight(b1, c1) + e1
		| CompoundBranch(a1, (b1, c1)), CompoundBranch(d1, (e1, f1)) -> total_weight(b1, c1) + total_weight(e1, f1)

let rec balanced: mobile -> bool =fun(lb,rb) -> match lb, rb with
		| SimpleBranch(a1, b1), SimpleBranch(a2, b2) -> if(a1*b1 = a2*b2) then true else false
    | SimpleBranch(a1, b1), CompoundBranch(c1, (d1, e1)) -> let x = a1*b1 in
																														let y = c1*total_weight(d1, e1) in
																														if(balanced(d1, e1)) then 
																														(if(x=y) then true else false) else false
    | CompoundBranch(a1, (b1, c1)), SimpleBranch(d1, e1) -> let x = a1*total_weight(b1, c1) in 
																														let y = d1*e1 in
																														if(balanced(b1, c1)) then 
																														(if(x = y) then true else false) else false
    | CompoundBranch(a1, (b1, c1)), CompoundBranch(d1, (e1, f1)) -> let x = a1*total_weight(b1, c1) in 
																																		let y = d1*total_weight(e1, f1) in
																																		if(balanced(b1, c1) && balanced(e1, f1)) then 
																																		(if(x=y) then true else false) else false

end 

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
type exp = 
  | V of name
  | P of name * exp
  | C of exp * exp
and name = string 

let rec checkWellFormed expression checkList = 
    match expression with 
    | V (u) -> List.mem u checkList
    | P (u,v) -> checkWellFormed v (u::checkList)
    | C (u,v) -> (checkWellFormed u checkList) && (checkWellFormed v checkList)

let check: exp -> bool = fun expression -> checkWellFormed expression []

end

(***********************************)
(**            Problem 3          **)
(***********************************)
module Problem3 = struct
  type program = exp
  and exp = 
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | LETREC of var * var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string

  type value = Int of int | Bool of bool 
             | Procedure of var * exp * env 
             | RecProcedure of var * var * exp * env
  and env = var -> value
  
  let empty_env = fun _ -> raise (Failure "Environment is empty")
  let extend_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_env e x = e x
  
  let run : program -> value
  =fun pgm -> Int 0 (* TODO *)
end

(***********************************)
(**            Problem 4          **)
(***********************************)

module Problem4 = struct
  type program = exp
  and exp = 
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
  and var = string
  
  type nl_program = nl_exp
  and nl_exp = 
    | NL_CONST of int
    | NL_VAR of int
    | NL_ADD of nl_exp * nl_exp
    | NL_SUB of nl_exp * nl_exp
    | NL_ISZERO of nl_exp
    | NL_IF of nl_exp * nl_exp * nl_exp
    | NL_LET of nl_exp * nl_exp
    | NL_PROC of nl_exp 
    | NL_CALL of nl_exp * nl_exp
  
  let translate : program -> nl_program
  =fun pgm -> NL_CONST 0 (* TODO *)
end

(***********************************)
(**            Problem 5          **)
(***********************************)

module Problem5 = struct
  open Problem4
  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list
  
  let nl_run : nl_program -> nl_value
  =fun pgm -> NL_Int 0 (* TODO *)
end
