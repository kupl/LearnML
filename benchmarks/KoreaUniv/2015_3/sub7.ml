(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)

type mobile = branch * branch
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec getWeight : mobile -> int
  = fun(lb, rb) ->
    (match lb with
        SimpleBranch(l, w) -> w
      | CompoundBranch(l, m) -> getWeight(m))
    +(match rb with
         SimpleBranch(l, w) -> w
       | CompoundBranch(l, m) -> getWeight(m))
;;
let rec balanced : mobile -> bool
  = fun(lb,rb) -> 
    match lb with
        SimpleBranch(l, w) ->
          (match rb with 
              SimpleBranch(l2, w2) ->
                (if(l*w = l2*w2) then true else false)
            | CompoundBranch(l2, m2) -> 
                if(balanced m2)
                then(if(l*w = l2*getWeight(m2)) then true else false)
                else false
          )
      | CompoundBranch(l, m) -> 
          (if(balanced m)
           then (match rb with 
                    SimpleBranch(l2, w2) ->
                      (if(l*getWeight(m) = l2*w2) then true else false)
                  | CompoundBranch(l2, m2) -> 
                      if(balanced m2)
                      then(if(l*getWeight(m) = l2*getWeight(m2)) then true else false)
                      else false
                )
           else false)
;;



(***********************************)
(**            Problem 2          **)
(***********************************)

type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let rec isinList a l = 
  match l with
      [] -> false
    | h::t -> if(h = a) then true else isinList a t
;;
let rec validCheck : exp -> var list -> bool
  = fun e l ->
    match e with
        V(_v) -> isinList _v l
      | P(_v, _e) -> let _l = _v::l in validCheck _e _l
      | C(_e1, _e2) -> validCheck _e1 l && validCheck _e2 l
;;
let check : exp -> bool
  = fun e -> validCheck e [];;



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
and env = (var * value) list

let empty_env = []
let rec apply_env x e =
  match e with
    | [] -> raise (Failure "Environment is empty")
    | (y,v)::tl -> if x = y then v else apply_env x tl
let extend_env (x,v) e = (x,v)::e

let rec eval : exp -> env -> value
  = fun exp env ->
    match exp with
      | CONST(n) -> Int(n)
      | VAR(x) -> apply_env x env
      | ADD(e1, e2) ->
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              | Int n1, Int n2 -> Int(n1 + n2)
              | _ -> raise (Failure "Type Error: non-numeric values"))
      | SUB(e1, e2) ->
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1, v2 with
              | Int n1, Int n2 -> Int(n1 - n2)
              | _ -> raise (Failure "Type Error: non-numeric values"))
      | ISZERO(e) ->
          (match eval e env with
            | Int n when n = 0 -> Bool true
            | _ -> Bool false)
      | IF(e1, e2, e3) ->
          (match eval e1 env with
            | Bool true -> eval e2 env
            | Bool false -> eval e3 env
            | _ -> raise (Failure "Type Error: condition must be Bool type"))
      | LET(x, e1, e2) ->
          let v1 = eval e1 env in
            eval e2 (extend_env (x, v1) env)
      | LETREC(f, x, e1, e2) ->
          eval e2 (extend_env (f, RecProcedure(f, x, e1, env)) env)
      | PROC(x, e) ->
          Procedure(x, e, env)
      | CALL(e1, e2) ->
          let v1 = eval e1 env in
          let v2 = eval e2 env in
            (match v1 with
                RecProcedure(f, x, e1, e2) ->
                  eval e1 (extend_env (x, v2) (extend_env (f, v1) e2))
              | Procedure(x, e1, e2) ->
                  eval e1 (extend_env (x, v2) e2)
              | _ -> raise (Failure "Type Error: expression must be procedure type"))
;;
let run : program -> value
  = fun pgm -> eval pgm empty_env
;;
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
and rep = var list

let empty_rep = []
let rec apply_rep x r =
  match r with
    | [] -> raise (Failure "Representation is empty")
    | y::tl -> if x = y then 0 else 1+apply_rep x tl
let extend_rep x r = x::r

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

let rec convert : exp -> rep -> nl_program
  = fun exp rep ->
    match exp with
      | CONST(n) -> NL_CONST(n)
      | VAR(x) -> NL_VAR(apply_rep x rep)
      | ADD(e1, e2) -> NL_ADD(convert e1 rep, convert e2 rep)
      | SUB(e1, e2) -> NL_SUB(convert e1 rep, convert e2 rep)
      | ISZERO(e) -> NL_ISZERO(convert e rep)
      | IF(e1, e2, e3) -> NL_IF(convert e1 rep, convert e2 rep, convert e3 rep)
      | LET(x, e1, e2) -> 
          let rep2 = extend_rep x rep
          in NL_LET(convert e1 rep, convert e2 rep2)
      | PROC(x, e) ->
          let rep2 = extend_rep x rep
          in NL_PROC(convert e rep2)
      | CALL(e1, e2) ->
          NL_CALL(convert e1 rep, convert e2 rep)
;;
let rec translate : program -> nl_program
  = fun pgm -> convert pgm empty_rep
;;


(***********************************)
(**            Problem 5          **)
(***********************************)

type nl_value = NL_Int of int 
              | NL_Bool of bool 
              | NL_Procedure of nl_exp * nl_env
and nl_env = nl_value list
let empty_nl_env = []
let rec apply_nl_env i e =
  match e with
    | [] -> raise (Failure "Environment is empty")
    | h::tl -> if i=0 then h else (apply_nl_env (i-1) tl)
let extend_nl_env x r = x::r


let rec nl_eval : nl_exp -> nl_env -> nl_value
  = fun exp env ->
    match exp with
      | NL_CONST(n) -> NL_Int(n)
      | NL_VAR(x) -> apply_nl_env x env
      | NL_ADD(e1, e2) ->
          let v1 = nl_eval e1 env in
          let v2 = nl_eval e2 env in
            (match v1, v2 with
              | NL_Int n1, NL_Int n2 -> NL_Int(n1 + n2)
              | _ -> raise (Failure "Type Error: non-numeric values"))
      | NL_SUB(e1, e2) ->
          let v1 = nl_eval e1 env in
          let v2 = nl_eval e2 env in
            (match v1, v2 with
              | NL_Int n1, NL_Int n2 -> NL_Int(n1 - n2)
              | _ -> raise (Failure "Type Error: non-numeric values"))
      | NL_ISZERO(e) ->
          (match nl_eval e env with
            | NL_Int n when n = 0 -> NL_Bool true
            | _ -> NL_Bool false)
      | NL_IF(e1, e2, e3) ->
          (match nl_eval e1 env with
            | NL_Bool true -> nl_eval e2 env
            | NL_Bool false -> nl_eval e3 env
            | _ -> raise (Failure "Type Error: condition must be Bool type"))
      | NL_LET(e1, e2) ->
          let v1 = nl_eval e1 env in
            nl_eval e2 (extend_nl_env v1 env)
      | NL_PROC(e) ->
          NL_Procedure(e, env)
      | NL_CALL(e1, e2) ->
          let v1 = nl_eval e1 env in
          let v2 = nl_eval e2 env in
            (match v1 with
                NL_Procedure(e1, e2) ->
                  nl_eval e1 (extend_nl_env v2 e2)
              | _ -> raise (Failure "Type Error: expression must be procedure type"))

;;

let nl_run : nl_program -> nl_value
  = fun pgm -> nl_eval pgm empty_nl_env
;;

