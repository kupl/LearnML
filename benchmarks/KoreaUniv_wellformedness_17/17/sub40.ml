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

type env = (var*exp) list

let rec apply_env e v =
 match e with
 |[] -> VAR v
 |(va,ex)::tl -> if v=va then ex else apply_env tl v

let rec extend_env (va,ex) e = (va,ex)::e

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)


let rec find_v : var -> exp -> bool
= fun var exp ->
 match exp with
  | CONST n -> false
  | VAR x -> if var = x then true else false
  | ADD(e1,e2) -> (find_v var e1)||(find_v var e2)
  | SUB(e1,e2) -> (find_v var e1)||(find_v var e2)
  | MUL(e1,e2) -> (find_v var e1)||(find_v var e2)
  | DIV(e1,e2) -> (find_v var e1)||(find_v var e2)
  | ISZERO e -> find_v var e
  | READ -> false
  | IF (e1,e2,e3) -> (find_v var e1)||(find_v var e2)||(find_v var e3)
  | LET (v,e1,e2) -> if ((find_v var e1)||(find_v var e2)) then true else false
  | LETREC (v1,v2,e1,e2)-> (find_v var e1)||(find_v var e2) 
  | PROC (v,e) -> find_v var e
  | CALL (e1,e2) -> (find_v var e1)||(find_v var e2)

(* You can define datatypes and helper functions as necessary *)
let rec expand1 : exp-> env -> exp 
= fun exp env ->
  match exp with
  | CONST n -> CONST n  
  | VAR x -> (apply_env env x)
  | ADD (e1,e2)-> ADD (expand1 e1 env, expand1 e2 env)
  | SUB (e1,e2)-> SUB (expand1 e1 env, expand1 e2 env)
  | MUL (e1,e2)-> MUL (expand1 e1 env, expand1 e2 env)
  | DIV (e1,e2)-> DIV (expand1 e1 env, expand1 e2 env)
  | ISZERO e -> ISZERO (expand1 e env)
  | READ -> CONST (read_int ())
  | IF (e1,e2,e3) -> IF (expand1 e1 env, expand1 e2 env, expand1 e3 env)
  | LET (v,e1,e2)->if (find_v v e2) then
     let envl= (extend_env (v,e1) env) in (expand1 e2 envl) else exp

  | LETREC (v1,v2,e1,e2) -> exp
  | PROC (v,e)-> PROC (v, expand1 e env)
  | CALL (e1,e2)-> CALL( expand1 e1 env, expand1 e2 env)

let rec expand : exp -> exp
= fun exp -> expand1 exp []

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type lenv = var list 

(*environment*)
let empty_lenv =[]
let extend_lenv v e = v::e
let rec apply_lenv e x =
 match e with
 |[] -> false 
 |v::tl -> if x = v then true  else apply_lenv tl x

let rec pcheck : lambda -> lenv -> bool
= fun lam e ->
match lam with 
|V v -> if (apply_lenv e v) then true else false
|P(v,l) -> let e1 =(extend_lenv v e)in  (pcheck l e1)            
|C(l1,l2) ->if (pcheck l1 e)&&(pcheck l2 e) then true else false 

let rec check : lambda -> bool
= fun lam ->
pcheck lam []
