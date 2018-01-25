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

  let rec total_weight : branch -> int
  =fun br -> match br with
	SimpleBranch (l, w) -> w
	|CompoundBranch (l, m) -> match m with
		(b1, b2) -> total_weight(b1) + total_weight(b2)
  
  let len : branch -> int
  =fun br -> match br with
	SimpleBranch (l, w) -> l
	|CompoundBranch (l, m) -> l
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> len(lb) * total_weight(lb) = len(rb) * total_weight(rb)
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec contain
	= fun str lst -> match lst with
		[] -> false
		|h::t -> if (h = str) then true else contain str t

  let rec chk
	= fun expr env -> match expr with
		V str -> contain str env
		|P (s, e) -> chk e (env@[s])
		|C (e1, e2) -> chk e1 env && chk e2 env
		
  let check : exp -> bool
  =fun e -> chk e []
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
  
module type Env = sig
  type t
  exception Not_found
  val empty : t
  val extend : var -> value -> t -> t
  val lookup : var -> t -> value
end

module Env : Env = struct
  type t = var -> value
  exception Not_found
  let empty = fun x-> prerr_endline x; raise Not_found
  let lookup x e = e x
  let extend x v e = fun y -> if x = y then v else (lookup y e)
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
  | VAR x -> Env.lookup x env
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
      eval e2 (Env.extend x v1 env)
  
  let run : program -> value
  =fun pgm -> eval pgm Env.empty
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
