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

  let rec cal_weight : mobile -> int
  = fun (lb,rb) -> match lb,rb with
  | SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> w1 + w2
  | SimpleBranch (l1,w1), CompoundBranch (l2,b2) -> w1 + cal_weight(b2)
  | CompoundBranch (l1,b1), SimpleBranch (l2,w2) -> w2 + cal_weight(b1)
  | CompoundBranch (l1,b1), CompoundBranch (l2,b2) -> cal_weight(b1) + cal_weight(b2)

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match lb,rb with
  | SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> if l1*w1 = l2*w2 then true else false
  | SimpleBranch (l1,w1), CompoundBranch (l2,b2) -> if balanced(b2) && (l1*w1 = l2*cal_weight(b2)) then true else false
  | CompoundBranch (l1,b1), SimpleBranch (l2,w2) -> if balanced(b1) && (l2*w2 = l1*cal_weight(b1)) then true else false
  | CompoundBranch (l1,b1), CompoundBranch (l2,b2) -> if balanced(b1) && balanced(b2) && (l1*cal_weight(b1) = l2*cal_weight(b2)) then true else false

  let one = balanced (CompoundBranch (3, 
  (CompoundBranch (2,
    (SimpleBranch (1,1),SimpleBranch (1,1)))
  ,SimpleBranch (1,4))), 
SimpleBranch (6,3))

end


(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec equi x l = match l with
  | [] -> true
  | hd::tl -> (x=hd) && (equi x tl)

  let rec check_r : exp -> string list -> bool
  =fun e env -> match e with
  | V var  -> if env = [] then false else equi var env
  | P (v,e) -> check_r e (v::env)
  | C (e1,e2) -> (check_r e1 env) && (check_r e2 env)
  
  let check : exp -> bool
  = fun e -> check_r e []

 

  let two = check (P ("a", V "a"))
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
  let extend x v e = (x,v)::e
(*   let extend_f f x e env = (f,RecProcedure (f,x,e,))::env *)
  let apply_env e x = e x

  let rec lookup x e = 
    match e with
    | [] -> raise Not_found
    | (y,v)::tl -> if x = y then v else lookup x tl
    | (f,rp)::tl -> if x = f then rp else lookup x tl
 (*  let rec lookup_f f n e =
    match e with
    | [] -> raise Not_found
    | [f;x;e;env]::tl -> if f = f1 then 
    let env' = extend x n e in
    let env'' = env@env' in
    eval e1 env''
  else lookup_f f n tl *)

  let rec eval_bop = fun bop e1 e2 env ->
  let v1 = eval e1 env in
  let v2 = eval e2 env in 
      (match v1, v2 with
      | Int n1, Int n2 -> Int (bop n1 n2)
      | _ -> raise (Failure "error!"))

  and eval = fun e env -> match e with
  | CONST n -> Int n
  | VAR x -> lookup x env
  | ADD (e1, e2) -> eval_bop (+) e1 e2 env
  | SUB (e1, e2) -> eval_bop (-) e1 e2 env
  | ISZERO e ->
    let v = eval e env in
       (match v with
       | Bool b -> raise (Failure "error")
       | Int n -> if n = 0 then Bool true else Bool false)
  | IF (e1, e2, e3) ->
    let v1 = eval e1 env in 
    (match v1 with
    | Int n -> raise (Failure "error")
    | Bool b -> if b then eval e2 env else eval e3 env)
  | LET (x, e1, e2) ->
    let v1 = eval e1 env in 
    let env' = extend x v1 env in
     eval e2 env'
(*   | LETREC (f, x, e1, env) -> extend_f f x e1 env *)
(*   | CALL (f, n) ->  *)
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
