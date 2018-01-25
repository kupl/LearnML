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

let rec weighbr : branch -> int
= fun b -> match b with
| SimpleBranch(len, wg) -> wg
| CompoundBranch(len, mb) -> weighmb(mb)

and weighmb : mobile -> int
= fun (lb,rb) -> weighbr(lb) + weighbr(rb)

let rec torqbr : branch -> int
= fun b -> match b with
| SimpleBranch(len, wg) -> len * wg
| CompoundBranch(len, mb) -> len * weighmb(mb)

let rec balbr : branch -> bool
= fun b -> match b with
| SimpleBranch(len, wg) -> true
| CompoundBranch(len, mb) -> balmb(mb)

and balmb : mobile -> bool
=fun (lb, rb) -> 
if (torqbr(lb) == torqbr(rb)) then balbr(lb)&&balbr(rb)
else false 
  
  let balanced : mobile -> bool
  =fun (lb,rb) -> balmb(lb,rb)
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string

let rec comblist = fun (l1,l2) -> match l1 with
| [] -> l2
| hd::tl -> hd::comblist(tl,l2)

let rec searchlist : (string * string list) -> bool
= fun (a, l) -> match l with
| [] -> false
| hd::tl -> if (hd = a) then true else searchlist(a, tl)

let rec complist : string list -> string list
= fun (l) -> match l with
| [] -> []
| hd::tl -> if (searchlist(hd, tl)) then complist(tl)
else hd::tl

let rec vars : exp -> string list
=fun e -> match e with
| V x -> [x]
| P (x, e1) -> vars(e1)
| C (e1, e2) -> comblist(vars(e1),vars(e2))

let rec used : exp -> string list
=fun e -> match e with
| V x -> []
| P (x, e1) -> x::used(e1)
| C (e1, e2) -> comblist(used(e1),used(e2))

let rec matchvar : (string list * string list) -> bool
=fun (vr, us) -> match vr,us with
| [],_ -> true
| hd::tl, us -> if (searchlist(hd, us)) then matchvar(tl, us)
else false
  
  let check : exp -> bool
  =fun e -> matchvar(complist(vars(e)), used(e))
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
    (let v = eval e env in
      match v with
      | Int n -> if n = 0 then Bool true else Bool false
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
let v = eval e2 (extend_env (f, RecProcedure (f,x,e1,env)) env) in v

| PROC (x,e) ->
Procedure (x,e,env)

| CALL (e1,e2) ->
(let t = eval e1 env in
match t with
| Procedure (x,e,penv) -> let v = eval e2 env
in eval e (extend_env (x, v) penv)

| RecProcedure (f,x,e,penv) ->
let et = eval e1 env
in let v = eval e2 env
in eval e (extend_env (f, et) (extend_env (x,v) penv))
)

  
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

let rec index : (string * string list) -> int
=fun (str, l) -> match l with
| [str] -> 0
| hd::tl -> if (hd=str) then 0
else 1+index (str,tl)

let rec trans : (program * string list) -> nl_program
=fun (pgm, l) -> match pgm with
| CONST n -> NL_CONST n
| VAR x -> NL_VAR(index(x,l))
| ADD (e1,e2) -> NL_ADD (trans(e1,l), trans(e2,l))
| SUB (e1,e2) -> NL_SUB (trans(e1,l), trans(e2,l))
| ISZERO e -> NL_ISZERO (trans(e,l))
| IF (e1,e2,e3) -> NL_IF(trans(e1,l), trans(e2,l), trans(e3,l))
| LET (x,e1,e2) -> NL_LET(trans(e1,l), trans(e2,x::l))
| PROC (x,e) -> NL_PROC(trans(e,x::l))
| CALL (e1,e2) -> NL_CALL(trans(e1,l), trans(e2,l))


  let translate : program -> nl_program
  =fun pgm -> trans(pgm,[])
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

 type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list

let nl_empty_env = []
let nl_extend_env v e = v::e
let rec nl_apply_env e n = match e with
| [] -> raise Not_found
| v::tl -> if n=0 then v
else nl_apply_env tl (n-1)


let rec nl_eval_bop : (int -> int -> int) -> nl_exp -> nl_exp -> nl_env -> nl_value
=fun op e1 e2 env ->
  let v1 = nl_eval e1 env in
  let v2 = nl_eval e2 env in
    (match v1,v2 with
    | NL_Int n1, NL_Int n2 -> NL_Int (op n1 n2)
    | _ -> raise (Failure "Type Error: non-numeric values"))

and nl_eval : nl_exp -> nl_env -> nl_value
=fun exp env ->
  match exp with
  | NL_CONST n -> NL_Int n
  | NL_VAR n -> nl_apply_env env n
  | NL_ADD (e1,e2) -> nl_eval_bop (+) e1 e2 env
  | NL_SUB (e1,e2) -> nl_eval_bop (-) e1 e2 env
  | NL_ISZERO e ->
    (let v = nl_eval e env in
      match v with
      | NL_Int n -> if n = 0 then NL_Bool true else NL_Bool false
      | _ -> NL_Bool false)
  | NL_IF (e1,e2,e3) ->
    (match nl_eval e1 env with
    | NL_Bool true -> nl_eval e2 env
    | NL_Bool false -> nl_eval e3 env
    | _ -> raise (Failure "Type Error: condition must be Bool type"))
  | NL_LET (e1,e2) ->
    let v1 = nl_eval e1 env in
      nl_eval e2 (nl_extend_env v1 env)

| NL_PROC (e) ->
NL_Procedure (e,env)

| NL_CALL (e1,e2) ->
(let t = nl_eval e1 env in
match t with
| NL_Procedure (e,penv) -> let v = nl_eval e2 env
in nl_eval e (nl_extend_env v penv)
)

  let nl_run : nl_program -> nl_value
  =fun pgm -> nl_eval pgm nl_empty_env
end