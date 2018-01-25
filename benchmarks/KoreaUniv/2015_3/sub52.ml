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
  
  let rec weight : branch -> int
  =fun (branch) -> match branch with
|SimpleBranch(a,b) -> b
|CompoundBranch(a,(b1,b2)) -> weight(b1) + weight(b2)

  let rec torque : branch -> int
  =fun (branch) -> match branch with
|SimpleBranch(a,b) -> a*b
|CompoundBranch(a,(b1,b2)) ->a*(weight(b1) + weight(b2))


  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match (lb,rb) with
|SimpleBranch(x,y),SimpleBranch(z,w) -> if torque (lb) = torque (rb) then true else false

|CompoundBranch(x,y),SimpleBranch(z,w) -> if torque (rb) = torque (lb)&&balanced(y) then true else false
|SimpleBranch(x,y), CompoundBranch(z,w) -> if torque (lb) = torque (rb) && balanced(w) then true else false
|CompoundBranch(x,y),CompoundBranch(z,w) -> if (torque(lb)=torque(rb) &&balanced(y) && balanced(w)) then true else false

end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let check : exp -> bool
  =fun e -> true (*match e with
|V "a"-> true *)
 
(*|P ("a",e1)-> match e1 with
            |V "a" -> true
            |P ("a",e2) -> check e *)
                
         
            
 
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
