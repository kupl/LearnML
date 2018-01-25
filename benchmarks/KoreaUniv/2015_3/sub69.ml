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

  let rec getweight : branch -> int
  =fun b -> match b with
    | SimpleBranch (len, w) -> w
    | CompoundBranch (len, (lb, rb)) -> getweight lb + getweight rb

  let rec balanced : mobile -> bool
  =fun (lb,rb) -> match (lb,rb) with
    | (SimpleBranch (llen, lw), SimpleBranch (rlen, rw)) -> if (llen*lw)=(rlen*rw) then true else false
    | (SimpleBranch (_,_), CompoundBranch (rlen, rw)) -> balanced (lb, (SimpleBranch (rlen, (getweight rb))))
    | (CompoundBranch (llen, lw), SimpleBranch (_,_)) -> balanced ((SimpleBranch (llen, (getweight lb))), rb)
    | (CompoundBranch (llen, lw), CompoundBranch (rlen, rw)) -> balanced((SimpleBranch (llen, (getweight lb))), (SimpleBranch (rlen, (getweight rb))))


end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec check : exp -> bool
  =fun e -> match e with
    | V x -> false
    | P (x,e1) -> (match e1 with
        | V y -> x=y
        | P (y,e2) -> (match e2 with
            | C (e3,e4) -> (check (P (x,e3)) || check (P (x,e4))) && (check (P (y,e3)) || check (P (y,e4)))
            | _ -> check (P (x,e2)) || check (P (y,e2)))
        | C (e2,e3) -> check (P (x,e2)) && check (P (x,e3)))
    | C (e1,e2) -> check e1 && check e2
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
 
  let rec eval_bop : (int -> int -> int) -> exp -> exp -> env -> value
  =fun op e1 e2 env ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
        (match v1,v2 with
          | Int n1, Int n2 -> Int (op n1 n2)
          | _ -> raise (Failure "Type Error: non-numeric values"))

  and eval : exp -> env -> value
  =fun exp env ->
    match exp with
      | CONST n -> Int n
      | VAR x -> apply_env env x
      | ADD (e1,e2) -> eval_bop (+) e1 e2 env
      | SUB (e1,e2) -> eval_bop (-) e1 e2 env
      | ISZERO e ->
          (match eval e env with
             | Int n when n = 0 -> Bool true
             | _ -> Bool false)
      | IF (e1,e2,e3) ->
          (match eval e1 env with
             | Bool true -> eval e2 env
             | Bool false -> eval e3 env
             | _ -> raise (Failure "Type Error: condition must be Bool type"))
      | LET (x,e1,e2) ->
          let v1 = eval e1 env in
            eval e2 (extend_env (x,v1) env)
      | LETREC (f,x,e1,e2) ->
          let rp = RecProcedure (f,x,e1,env) in
            eval e2 (extend_env (f,rp) env)
      | PROC (x,e) -> Procedure (x,e,env)
      | CALL (e1,e2) ->
          let v1 = eval e2 env in
            (match eval e1 env with
               | Procedure (x,e,env1) -> eval e (extend_env (x,v1) env1)
               | RecProcedure (f,x,e,env1) ->
                   let rp = RecProcedure (f,x,e,env1) in
                     eval e (extend_env (x,v1) (extend_env (f,rp) env1))
               | _ -> raise (Failure "Error: first expression must be Procedure or RecProcedure"))

  let run : program -> value
  =fun pgm -> eval pgm empty_env
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
 
  let rec find : string list -> string -> int -> int
  =fun l x start -> match l with
    | [] -> raise (Failure "Error: Not found")
    | hd::tl -> if hd=x then start else find tl x (start+1)

  let rec translation : program -> string list -> nl_program
  =fun pgm l -> match pgm with
    | CONST n -> NL_CONST n
    | VAR x -> NL_VAR (find l x 0)
    | ADD (e1,e2) -> NL_ADD (translation e1 l, translation e2 l)
    | SUB (e1,e2) -> NL_SUB (translation e1 l, translation e2 l)
    | ISZERO e -> NL_ISZERO (translation e l)
    | IF (e1,e2,e3) -> NL_IF (translation e1 l, translation e2 l, translation e3 l)
    | LET (x,e1,e2) -> NL_LET (translation e1 l, translation e2 (x::l))
    | PROC (x,e) -> NL_PROC (translation e (x::l))
    | CALL (e1,e2) -> NL_CALL (translation e1 l, translation e2 l)

  let translate : program -> nl_program
  =fun pgm -> translation pgm []
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

  let rec eval_bop : (int -> int -> int) -> nl_exp -> nl_exp -> nl_env -> nl_value
  =fun op e1 e2 env ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
        (match v1,v2 with
           | NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
           | _ -> raise (Failure "Type Error: non-numeric values"))

  and eval : nl_exp -> nl_env -> nl_value
  =fun exp env ->
    match exp with
      | NL_CONST n -> NL_Int n
      | NL_VAR n -> if n >= 0 && n < List.length env then List.nth env n
                    else raise (Failure "Not Found")
      | NL_ADD (e1,e2) -> eval_bop (+) e1 e2 env
      | NL_SUB (e1,e2) -> eval_bop (-) e1 e2 env
      | NL_ISZERO e ->
        (match eval e env with
           | NL_Int n when n = 0 -> NL_Bool true
           | _ -> NL_Bool false)
      | NL_IF (e1,e2,e3) ->
        (match eval e1 env with
           | NL_Bool true -> eval e2 env
           | NL_Bool false -> eval e3 env
           | _ -> raise (Failure "Type Error: condition must be Bool type"))
      | NL_LET (e1,e2) ->
        let v1 = eval e1 env in
          eval e2 (v1::env)
      | NL_PROC e -> NL_Procedure (e,env)
      | NL_CALL (e1,e2) ->
          let v1 = eval e2 env in
            (match eval e1 env with
               | NL_Procedure (e,env1) -> eval e (v1::env1)
               | _ -> raise (Failure "Error: first expression must be NL_Procedure"))

  let nl_run : nl_program -> nl_value
  =fun pgm -> eval pgm []
end
