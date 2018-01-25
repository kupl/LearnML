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

let rec balanced : mobile -> bool
=fun (lb, rb) -> match (lb, rb) with 
|(SimpleBranch (_, _), SimpleBranch (_, _))->if ((getForce (lb) = getForce (rb)) ) then true else false
|(SimpleBranch (_,_), CompoundBranch (_,a))->if ((getForce (lb) = getForce (rb)) && balanced(a)) then true else false
|(CompoundBranch (_,a), SimpleBranch (_,_))->if ((getForce (lb) = getForce (rb)) && balanced(a)) then true else false
|(CompoundBranch (_,a), CompoundBranch (_,b))->if ((getForce (lb) = getForce (rb))&& balanced(a) && balanced(b)) then true else false
and getWeight : branch -> int
=fun (mb) -> match mb with 
|SimpleBranch (a, b) -> b
|CompoundBranch (a, (b, c)) -> getWeight(b)+getWeight(c)
and getForce : branch -> int
=fun (mb) -> match mb with 
|SimpleBranch (a, b) -> a*b
|CompoundBranch (a, (b, c)) ->a*(getWeight(b)+getWeight(c))
end
(***********************************)
(**            Problem 2          **)
(***********************************)
module Problem2 = struct
type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let rec findTarget tar lst= 
match lst with 
|[] ->false
|hd::tl-> if hd=tar then true else findTarget tar tl;;

let rec isBound : ((var list) * exp)->bool
=fun(x,y) -> match y with 
|V c -> findTarget c x
|P(c, d)-> isBound(c::x, d)
|C(c, d)-> isBound(x, c) && isBound(x, d);;
let rec check : exp -> bool
=fun e -> isBound([], e);;
end
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

let rec eval : exp->env->value
=fun exp env ->
match exp with 
|CONST n->Int n
|VAR x -> apply_env env x
|ADD (e1, e2)->
let v1 = eval e1 env in
let v2 = eval e2 env in
(match v1, v2 with
 |Int n1, Int n2 -> Int(n1+n2)
 |_->raise (Failure "Type Error : non-numeric values"))
|SUB (e1, e2)->
let v1 = eval e1 env in 
let v2 = eval e2 env in
(match v1, v2 with
 |Int n1, Int n2 -> Int(n1-n2)
 |_->raise (Failure "Type Error : non-numeric values"))
|ISZERO e ->
(match eval e env with
 |Int n when n =0 ->Bool true
 |_->Bool false)
|IF (e1, e2, e3)->
(match eval e1 env with
 |Bool true -> eval e2 env
 |Bool false -> eval e3 env
 |_ -> raise (Failure "Type Error : condition must be Bool type"))
|LET(x, e1, e2)->
let v1 = eval e1 env in 
eval e2 (extend_env (x, v1) env)

|LETREC (f, x, e1, e2)->
eval e2 (extend_env (f,RecProcedure(f, x, e1, env)) env)

|PROC (x, e1)->Procedure (x, e1, env) 

|CALL (e1, e2)-> 
(match eval e1 env with 
 |Procedure (x, e, env1)-> 
 let v= eval e2 env in
    eval e (extend_env (x, v) env1)
 |RecProcedure (f, x, e, env1)->
 let v = eval e2 env in
 eval e (extend_env (x, v)  (extend_env (f, RecProcedure(f, x, e, env1)) env1) )
 |_->raise(Failure "Type Error") )
let run : program -> value
=fun pgm -> eval pgm empty_env (* TODO *)
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

let rec countDepth v lst=
match lst with 
|[]->raise(Failure "not Bound")
|hd::tl->if hd=v then 0 else 1+countDepth v tl

let rec transform : program-> var list-> nl_program
=fun pgm lst-> match pgm with 
  | CONST n-> NL_CONST n
  | VAR x-> NL_VAR (countDepth x lst)
  | ADD (e1, e2) -> NL_ADD(transform e1 lst, transform e2 lst)
  | SUB (e1, e2) -> NL_SUB(transform e1 lst, transform e2 lst)
  | ISZERO e-> NL_ISZERO(transform e lst)
  | IF (e1, e2, e3) -> NL_IF(transform e1 lst, transform e2 lst, transform e3 lst)
  | LET (x, e1, e2) -> NL_LET(transform e1 (lst), transform e2 (x::lst))
  | PROC (x, e1) -> NL_PROC(transform e1 (x::lst))
  | CALL (e1, e2) -> NL_CALL (transform e1 lst, transform e2 lst)


let rec translate : program -> nl_program
=fun pgm-> transform pgm [] 
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

let rec getElement : int->nl_env->nl_value=
fun num env->
match env with
|[]->raise(Failure "overFlow")
|hd::tl->if num =0 then hd else getElement (num-1) tl

let rec nl_eval : nl_program->nl_env->nl_value=
fun pgm env-> match pgm with
| NL_CONST a ->NL_Int a
| NL_VAR a -> getElement a env
|NL_ADD (e1, e2)->
let v1 = nl_eval e1 env in
let v2 = nl_eval e2 env in
(match v1, v2 with
 |NL_Int n1, NL_Int n2 -> NL_Int(n1+n2)
 |_->raise (Failure "Type Error : non-numeric values"))
|NL_SUB (e1, e2)->
let v1 = nl_eval e1 env in 
let v2 = nl_eval e2 env in
(match v1, v2 with
 |NL_Int n1, NL_Int n2 -> NL_Int(n1-n2)
 |_->raise (Failure "Type Error : non-numeric values"))
|NL_ISZERO e ->
(match nl_eval e env with
 |NL_Int n when n =0 ->NL_Bool true
 |_->NL_Bool false)
|NL_IF (e1, e2, e3)->
(match nl_eval e1 env with
 |NL_Bool true -> nl_eval e2 env
 |NL_Bool false -> nl_eval e3 env
 |_ -> raise (Failure "Type Error : condition must be Bool type"))
|NL_LET(e1, e2)->
let v1 = nl_eval e1 env in 
nl_eval e2 (v1::env)

|NL_PROC (e1)->NL_Procedure (e1, env) 

|NL_CALL (e1, e2)-> 
(match nl_eval e1 env with 
 |NL_Procedure (e, env1)-> 
 let v= nl_eval e2 env in
   nl_eval e (v::env1)
 |_->raise(Failure "Type Error") )

let rec nl_run : nl_program -> nl_value
=fun pgm -> nl_eval pgm []
    end
