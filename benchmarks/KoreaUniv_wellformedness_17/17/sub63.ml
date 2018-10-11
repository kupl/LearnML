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


let rec isin : var -> exp -> bool = fun v ex -> match ex with
|CONST n -> false
|VAR x -> if x = v then true else false
|ADD (e1,e2) -> (isin v e1) || (isin v e2)
|SUB (e1,e2) -> (isin v e1) || (isin v e2)
|MUL (e1,e2) -> (isin v e1) || (isin v e2)
|DIV (e1,e2) -> (isin v e1) || (isin v e2)
|ISZERO e1 -> isin v e1
|READ -> false
|IF (e1, e2, e3) -> (isin v e1) || (isin v e2) || (isin v e3)
|PROC (x, e1) -> if x = v then false else isin v e1
|CALL (e1, e2) -> (isin v e1) || (isin v e2)
|LET (v1, e1, e2) -> if v = v1 then false else (isin v e1) || (isin v e2)
|LETREC (f, x, e1, e2) -> if f = v then false else (isin v e1) && (x != v) || (isin v e2)

type isenv =
|ENV of exp
|NOTENV
and env = (var * exp) list

let extend_env (x,v) e = (x,v)::e

let rec apply_env : var -> env -> isenv = fun v env ->
match env with
|[] -> NOTENV
|(v1, e1)::tl -> if v = v1 then ENV e1 else apply_env v tl

let rec del_env : var -> env -> env = fun v env ->
match env with
|[] -> []
|(v1, e1)::tl -> if v = v1 then tl else (v1, e1)::(del_env v tl)

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> let rec expand_second : exp -> env -> exp = fun ex env ->
match ex with
|CONST n -> CONST n
|ADD (e1,e2) -> ADD((expand_second e1 env), (expand_second e2 env))
|SUB (e1,e2) -> SUB((expand_second e1 env), (expand_second e2 env))
|MUL (e1,e2) -> MUL((expand_second e1 env), (expand_second e2 env))
|DIV (e1,e2) -> DIV((expand_second e1 env), (expand_second e2 env))
|ISZERO e1 -> ISZERO (expand_second e1 env)
|READ -> READ
|IF (e1, e2, e3) -> IF((expand_second e1 env), (expand_second e2 env), (expand_second e3 env))
|PROC (x, e1) -> let env1 = del_env x env in PROC (x, (expand_second e1 env1))
|CALL (e1, e2) -> CALL((expand_second e1 env), (expand_second e2 env))
|LET (v, e1, e2) -> if (isin v e2) then let e3 = expand_second e1 env in let env1 = extend_env (v, e3) env in expand_second e2 env1
else LET(v, (expand_second e1 env), (expand_second e2 env))
|VAR x -> let isenv1 = apply_env x env in (match isenv1 with
|ENV e1 -> e1
|NOTENV -> VAR x)
|LETREC (f, x, e1, e2) -> let env1 = del_env f env in let env2 = del_env x env1 in LETREC (f, x, (expand_second e1 env2), (expand_second e2 env1))
in let env0 = [] in let e1 = expand_second exp env0 in let e2 = expand_second e1 env0 in if e1 = e2 then e1 else expand e1


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec find x env =
match env with
|[] -> false
|hd::tl -> if hd = x then true else find x tl in
let rec del x env =
match env with
|[] -> []
|v::tl -> if v = x then tl else v::(del x tl) in
let rec free l env =
match l with
|V v -> if (find v env) then env else v::env
|P (v, l1) -> del v (free l1 env)
|C (l1, l2) -> let env1 = free l1 env in free l2 env1
in let env0 = [] in if (free lam env0) = [] then true else false

