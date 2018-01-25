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