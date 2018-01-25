(***********************************)
(**            Problem 1          **)
(***********************************)

type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
=fun (lb,rb) -> false (* TODO *)


(***********************************)
(**            Problem 2          **)
(***********************************)

type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let check : exp -> bool
=fun e -> true
 
(type var = string
 type exp = Var of var
 | P of var * exp
 | C of exp * exp
let id : exp = P ("x", Var "x")
 let w : exp = 
C (P ("x", C (Var "x", Var "x")), 
	P ("x", C (Var "x", Var "x")))
let check : exp -> bool
=fun e -> true
)

(***********************************)
(**            Problem 3          **)
(***********************************)
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

(***********************************)
(**            Problem 4          **)
(***********************************)

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

(***********************************)
(**            Problem 5          **)
(***********************************)

type nl_value = NL_Int of int 
              | NL_Bool of bool 
              | NL_Procedure of nl_exp * nl_env
and nl_env = nl_value list

let nl_run : nl_program -> nl_value
=fun pgm -> NL_Int 0 (* TODO *)

