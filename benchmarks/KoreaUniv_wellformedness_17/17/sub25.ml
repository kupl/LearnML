(**********************)
(*   Problem 1        *)
(**********************)

type exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)


(* You can define datatypes and helper functions as necessary *)
(*type value = 
	exp of exp
  | Closure of var * exp * env 
  | RecClosure of var * var * exp * env*)
type env = (var * exp) list

(* environment *)
let extend_env (x,v) e = (x,v)::e
let rec apply_env : env->var->exp = fun e x ->
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

let rec exist_env : env->var->bool = fun e x ->
  match e with
  | [] -> false
  | (y,v)::tl -> if x = y then true else exist_env tl x

let rec except_env : env->var->env = fun e x ->
  match e with
  | [] -> []
  | (y,v)::tl -> if x = y then except_env tl x else (y,v)::except_env tl x

let rec exist : exp->var->bool
= fun exp x -> match exp with
	|CONST n->false
	|VAR y-> if y=x then true else false
	|ADD (e1,e2) -> (exist e1 x)||(exist e2 x)
	|SUB (e1,e2) -> (exist e1 x)||(exist e2 x)
	|MUL (e1,e2) -> (exist e1 x)||(exist e2 x)
	|DIV (e1,e2) -> (exist e1 x)||(exist e2 x) 
	|ISZERO e1 -> exist e1 x
 	|READ -> false
 	|IF (e1,e2,e3)-> (exist e1 x)||(exist e2 x)||(exist e3 x) 
 	|LET (e1,e2,e3)-> (exist e2 x)||(exist e3 x) 
 	|LETREC (f,y,e1,e2)-> (exist e1 x)||(exist e2 x)
 	|PROC (y,e)-> if x=y then false else exist e x
 	|CALL (e1,e2)-> (exist e1 x)||(exist e2 x)

let rec poly :exp->env->exp
= fun exp env -> match exp with 
	|CONST n->CONST n
	|VAR x-> if (exist_env env x) then (apply_env env x) else (VAR x) (*찾는데 없다고 에러내는게 아닌 없으면 그대로 출력...let bound var만 교체*)
	|ADD (e1,e2) -> ADD (poly e1 env,poly e2 env) 
	|SUB (e1,e2) -> SUB (poly e1 env,poly e2 env) 
	|MUL (e1,e2) -> MUL (poly e1 env,poly e2 env) 
	|DIV (e1,e2) -> DIV (poly e1 env,poly e2 env) 
	|ISZERO e1 -> ISZERO (poly e1 env)
 	|READ -> let i = read_int() in CONST i
 	|IF (e1,e2,e3)-> IF (poly e1 env,poly e2 env,poly e3 env)
 	|LET (e1,e2,e3)->
 			let s = extend_env (e1,poly e2 env) env in
 			let k = poly e3 s in
 			if (exist e3 e1)=true then k
 				else LET (e1,e2,k)
 	|LETREC (f,x,e1,e2)->
 		let newenv = if exist_env env f then except_env env f else env in (*기존에 f가 있으면 배제*)
 		let s = extend_env (f,poly e1 newenv) newenv in
 		let k = poly e2 s in
 		if (exist e2 f)=true then k else LETREC (f,x,e1,k)
 	|PROC (x,e)->let de = if exist_env env x then except_env env x else env in
 		 PROC (x,(poly e de))
 	|CALL (e1,e2)-> 
 		(match e1 with
 			|VAR a-> if (exist_env env a) then
 				let s = (apply_env env a) in
 				CALL (s,poly e2 env)(*Static data type이므로 굳이 여기서 판단x*)
 			 else CALL(e1,poly e2 env)
 			|PROC (x,e)->PROC (x,poly e env) 
 			|_->CALL(e1,e2)
 			) (*여기서 e2는 poly하는거 맞음?*)

let rec expand : exp -> exp 
= fun exp -> poly exp []



(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda


type bounded = var list

let update_bounded a stk = a::stk;;
let rec find_bounded a stk = match stk with
|[]->false
|hd::tl->if hd=a then true else find_bounded a tl

let rec boundCheck : lambda -> bounded -> bool
= fun lam bd -> match lam with
|V v->find_bounded v bd
|P (v,l)-> let s = update_bounded v bd in boundCheck l s
|C (l1,l2)-> if boundCheck l2 bd then boundCheck l2 bd else false


let rec check : lambda -> bool
= fun lam -> boundCheck lam []




