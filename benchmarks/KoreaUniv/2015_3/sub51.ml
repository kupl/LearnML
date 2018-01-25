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
  
let rec totalWeight m : mobile -> int
  =fun m -> match m with 
  	| (SimpleBranch(_,w1), SimpleBranch(_,w2)) -> w1 + w2
  	| (CompoundBranch(_,m1), SimpleBranch(_,w2)) -> (totalWeight m1) + w2
  	| (SimpleBranch(_,w1), CompoundBranch(_,m2)) -> (totalWeight m2) + w1
  
  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match lb, rb with
  	| (SimpleBranch(l1,w1), SimpleBranch(l2,w2)) -> if (l1 * w1) = (l2 * w2) then true else false
  	| (CompoundBranch(l1,m1), SimpleBranch(l2,w2)) -> if (l1 * (totalWeight m1)) = (l2 * w2) then true else false 
  	| (SimpleBranch(l1,w1), CompoundBranch(l2,m2)) -> if (l1 * w1) = (l2 * (totalWeight m2)) then true else false;;
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
  =fun e ->
    let rec helpCheck exp var_list = 
    match exp with
      | V var -> List.mem var var_list
      | P (var, e1) -> helpCheck e1 (var::var_list)
      | C (e1, e2) -> helpCheck e1 var_list && helpCheck e2 var_list
    in helpCheck e [] ;;
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
  
  module type Env = sig
  type t
  exception Not_found
  val empty_env : t
  val extend_env : (var * value) -> t -> t
  val apply_env : t -> var -> value
end

module Env : Env = struct
  type t = var -> value
  exception Not_found
  let empty_env = fun _ -> raise (Failure "Environment is empty")
  let extend_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_env e x = e x
end

let rec eval_bop : (int -> int -> int) -> exp -> exp -> Env.t -> value
=fun op e1 e2 env ->
  let v1 = eval e1 env in
  let v2 = eval e2 env in
    (match v1,v2 with
    | Int n1, Int n2 -> Int (op n1 n2)
    | _ -> raise (Failure "Type Error: non-numeric values"))

and eval : exp -> Env.t -> value
=fun exp env ->
  match exp with
  | CONST n -> Int n
  | VAR x -> Env.apply_env env x
  | ADD (e1,e2) -> eval_bop (+) e1 e2 env
  | SUB (e1,e2) -> eval_bop (-) e1 e2 env
  | ISZERO e ->
    (let v = eval e env in
      match v with
      | Bool _ -> raise (Failure "Type Error: subexpression of zero? must be Int type")
      | Int n -> if n = 0 then Bool true else Bool false)
  | IF (e1,e2,e3) ->
    (match eval e1 env with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise (Failure "Type Error: condition must be Bool type"))
  | LET (x,e1,e2) ->
    let v1 = eval e1 env in
      eval e2 (Env.extend_env (x,v1) env)
  | LETREC
  | PROC (x,e) -> (x, e, env)
  | CALL (e1, e2) -> 

let run : program -> value
=fun pgm -> eval pgm Env.empty_env;;
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
  
module type Env = sig
  type t
  exception Not_found
  val empty_nl_env : t
  val extend_nl_env : (nl_value list) -> t -> t
  val apply_nl_env : t ->  nl_value list
end

module Env : Env = struct
  type t = nl_value list
  exception Not_found
  let empty_nl_env = fun _ -> raise (Failure "Environment is empty")
  let extend_nl_env (x,v) e = fun y -> if x = y then v else (e y)
  let apply_nl_env e x = e x
end

let rec eval_bop : (int -> int -> int) -> exp -> exp -> Env.t -> nl_value
=fun op e1 e2 nl_env ->
  let v1 = eval e1 nl_env in
  let v2 = eval e2 nl_env in
    (match v1,v2 with
    | NL_INT n1, NL_INT n2 -> NL_INT (op n1 n2)
    | _ -> raise (Failure "Type Error: non-numeric nl_values"))

and eval : exp -> Env.t -> nl_value
=fun exp nl_env ->
  match exp with
  | CONST n -> NL_INT n
  | VAR x -> Env.apply_nl_env nl_env x
  | ADD (e1,e2) -> eval_bop (+) e1 e2 nl_env
  | SUB (e1,e2) -> eval_bop (-) e1 e2 nl_env
  | ISZERO e ->
    (let v = eval e nl_env in
      match v with
      | BL_NL_Bool _ -> raise (Failure "Type Error: subexpression of zero? must be NL_INT type")
      | NL_INT n -> if n = 0 then BL_NL_Bool true else BL_NL_Bool false)
  | IF (e1,e2,e3) ->
    (match eval e1 nl_env with
    | BL_NL_Bool true -> eval e2 nl_env
    | BL_NL_Bool false -> eval e3 nl_env
    | _ -> raise (Failure "Type Error: condition must be BL_NL_Bool type"))
  | LET (x,e1,e2) ->
    let v1 = eval e1 nl_env in
      eval e2 (Env.extend_nl_env (x,v1) nl_env)
  | PROC (x,e) -> (x, e, nl_env)
  | CALL (e1, e2) -> 

let run : program -> nl_value
=fun pgm -> eval pgm Env.empty_nl_env;;

end
