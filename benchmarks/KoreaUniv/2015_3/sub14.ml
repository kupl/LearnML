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

let rec weight: mobile -> int
= fun (lb, rb) ->
match lb with
SimpleBranch (l, w) -> (match rb with
	SimpleBranch (l1, w1) -> (w + w1)
	|CompoundBranch (l1, m) -> (w + (weight m)))
|CompoundBranch (l, m) -> (match rb with
	SimpleBranch (l1, w1) -> ((weight m) + w1)
	|CompoundBranch (l1, m1) -> ((weight m) + (weight m1)))

let rec balanced : mobile -> bool
=fun (lb,rb) ->
match lb with
SimpleBranch (l, w) -> (match rb with
	SimpleBranch (l1, w1) -> if (l * w)=(l1 * w1) then true else false
	|CompoundBranch (l, m) -> if (balanced m)=false then false
		else (match rb with
		SimpleBranch (l2, w2) -> if (l2 * w2)=(l * (weight m)) then true else false
		|CompoundBranch(l3, m1) -> if (balanced m1)=false then false
		else if (l * (weight m)) = (l3 * (weight m1)) then true else false
	)
)
|CompoundBranch (l, m) -> if (balanced m)=false then false
else (match rb with
	SimpleBranch (l1, w1) -> if (l * (weight m))=(l1 * w1) then true else false
	|CompoundBranch(l1, m1) -> if (balanced m1)=false then false
		else if (l * (weight m))=(l1 * (weight m1)) then true else false)

end
(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let rec envmt : exp -> string list
= fun e ->
let lst =[] in
match e with 
V v -> lst
|P (v, expr) -> v::(envmt expr)@lst
|C (exp1, exp2) -> (envmt exp1)@(envmt exp2)@lst

let rec exist : exp * string list -> int 
= fun (e, lst) ->
match e with
V v -> (match lst with
	[] -> 0
	|hd::tl -> if hd = v then 1 + (exist (e,tl)) else (exist (e, tl)))
|P (v, exp1) -> (exist (exp1, lst))
|C (exp1, exp2) -> (exist (exp1, lst)) +  (exist (exp2, lst))

let check : exp -> bool
=fun e -> 
let env = (envmt e) in
if (exist (e, env)) = 0 then false
else if List.length env > (exist (e, env)) then false
else true
	
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
let create_env (f, x, e1, e) = extend_env (f, RecProcedure(f, x, e1, e)) e

let rec eval_bop: (int->int->int) -> program -> program -> env -> value
= fun op e1 e2 env ->
let v1 = eval e1 env in
let v2 = eval e2 env in
(match v1, v2 with
Int n1, Int n2 -> Int (op n1 n2)
|_-> raise (Failure "Type error"))

and eval: program -> env -> value
= fun exp env ->
let func = [] in
match exp with
CONST n -> Int n
|VAR x -> apply_env env x
|ADD (e1, e2) -> eval_bop (+) e1 e2 env
|SUB (e1, e2) -> eval_bop (-) e1 e2 env
|ISZERO e -> (let v = eval e env in
	match v with
	Int n -> if n = 0 then Bool true else Bool false
	|_ -> raise (Failure "Type error in ISZERO"))
|IF(e1, e2, e3) ->	(match eval e1 env with
	Bool true -> eval e2 env
	|Bool false -> eval e3 env
	|_-> raise (Failure "Type error in IF"))
|LET (x, e1, e2) -> let v1 = eval e1 env in eval e2 (extend_env (x, v1) env)
|LETREC (f, x, e1, e2) -> eval e2 (create_env (f, x, e1, env))
|PROC (x, e1) -> Procedure (x, e1, env) 
|CALL (e1, e2) -> let v2 = eval e2 env in
	(match (eval e1 env) with
	Procedure (x, e, envr) -> eval e (extend_env (x, v2) envr)
	|RecProcedure (f, x, e, envr)-> let envr1 = extend_env (x, v2) envr in
		eval e (create_env (f, x, e, envr1))
	|_-> raise (Failure "error in call"))

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

let rec exist_v: var * string list -> int
= fun (v, l) ->
match l with
[] -> 0
|hd::tl -> if v = hd then 1 else exist_v(v, tl)

(*check if new var is in the var list, if not add to the existing list*)
let rec newlist: string list * string list -> string list
= fun (l1, l2) ->
match l1 with
[] -> l2
|hd::tl -> if exist_v(hd, l2) = 0 then let new_l2 = hd::l2 in newlist(tl, new_l2) 
	else newlist(tl, l2)

(*use to check the place of the variable in the env list*)
let rec find: string * string list -> int
= fun (v, lst) -> 
match lst with 
[] -> 0
|hd::tl -> if hd = v then 0 else 1 + (find (v, tl))

let rec eval: program * string list -> nl_program
= fun (exp, lst) ->
match exp with 
CONST n -> NL_CONST n
|VAR v -> NL_VAR (find (v, lst))
|ADD(e1, e2) -> NL_ADD(eval (e1, lst), eval (e2, lst))
|SUB(e1, e2) -> NL_SUB(eval (e1, lst), eval (e2, lst))
|ISZERO e1 -> NL_ISZERO(eval (e1, lst))
|IF (e1, e2, e3) -> NL_IF(eval (e1, lst), eval(e2, lst), eval (e3, lst))
|LET (v, e1, e2) -> NL_LET (eval (e1, lst), eval (e2, newlist([v], lst)))
|PROC(v, e1) -> NL_PROC (eval(e1, newlist([v], lst)))
|CALL(e1, e2) -> NL_CALL (eval(e1, lst), eval(e2, lst))

let translate : program -> nl_program
=fun p -> eval (p, []) 

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

let rec find_n: int * nl_env -> nl_value
= fun (n, env) ->
match env with
[] -> raise (Failure "error")
|hd::tl -> if n=0 then hd else find_n((n-1), tl)

let rec get: nl_program * nl_env -> nl_value
= fun (pgm, env) ->
match pgm with
NL_CONST n -> NL_Int n
|NL_VAR n -> find_n (n, env)
|NL_ADD (p1, p2) -> (match get (p1, env), get (p2, env) with
	NL_Int n1, NL_Int n2 -> NL_Int (n1+n2)
	|_ -> raise (Failure "error in NL_ADD"))
|NL_SUB (p1, p2) -> (match get (p1, env), get (p2, env) with
	NL_Int n1, NL_Int n2  -> NL_Int (n1-n2)
	|_-> raise (Failure "error in NL_SUB"))
|NL_ISZERO p1 -> (match get(p1, env) with
	NL_Int n -> if n=0 then NL_Bool true else NL_Bool false
	|_-> raise (Failure "error in NL_ISZERO"))
|NL_IF (p1, p2, p3) -> (match get(p1, env) with
	NL_Bool tf -> if tf=true then get(p2, env) else get(p3, env)
	|_-> raise (Failure "error in NL_IF"))
|NL_LET (p1, p2) -> let v1 = get (p1, env) in get (p2, v1::env)
|NL_PROC p1 -> NL_Procedure (p1, env)
|NL_CALL (p1, p2) -> let v2 = get (p2, env) in let v1 = get (p1, env) in 
	(match v1 with
	NL_Procedure (p_v1, env_v1) -> get (p_v1, v2::env_v1)
	|_-> raise (Failure "error in NL_CALL"))


let nl_run : nl_program -> nl_value
=fun pgm -> get (pgm, [])


end	
