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
let rec expand : exp -> exp 
= fun exp -> 
let rec occurcheck v exp =
  match exp with
  | CONST n -> false
  | VAR x -> if x = v then true else false
  | ADD (e1, e2) -> (occurcheck v e1) || (occurcheck v e2)
  | SUB (e1, e2) -> (occurcheck v e1) || (occurcheck v e2)
  | MUL (e1, e2) -> (occurcheck v e1) || (occurcheck v e2)
  | DIV (e1, e2) -> (occurcheck v e1) || (occurcheck v e2)
  | ISZERO e1 -> occurcheck v e1
  | READ -> false
  | IF (e1, e2, e3) -> (occurcheck v e1) || (occurcheck v e2) || (occurcheck v e3)
  | LET (x, e1, e2) -> if x = v then occurcheck v e1
                       else (occurcheck v e1) || (occurcheck v e2)
  | LETREC (f, x, e1, e2) -> if f = v then false else if x = v then occurcheck v e2 else (occurcheck v e1) || (occurcheck v e2)
  | PROC (x, e1) -> if x = v then false else occurcheck v e1
  | CALL (e1, e2) -> (occurcheck v e1) || (occurcheck v e2) 
in
let rec save_letdef var exp env = 
match env with
| [] -> [(var, exp)]
| (v, e)::tl -> if v = var then (var, exp)::tl else (v,e)::(save_letdef var exp tl)
in
let rec find_var var env =
match env with
| [] -> VAR var
| (v, e)::tl -> if var = v then e else (find_var var tl)
in
let rec eval exp env =
match exp with
| CONST n -> CONST n
| VAR x -> find_var x env
| ADD (e1, e2) -> ADD (eval e1 env, eval e2 env)
| SUB (e1, e2) -> SUB (eval e1 env, eval e2 env)
| MUL (e1, e2) -> MUL (eval e1 env, eval e2 env)
| DIV (e1, e2) -> DIV (eval e1 env, eval e2 env)
| ISZERO e -> ISZERO (eval e env)
| READ -> READ
| IF (e1, e2, e3) -> IF (eval e1 env, eval e2 env, eval e3 env)
| LET (x, e1, e2) -> if occurcheck x e2 then eval e2 (save_letdef x e1 env) else LET (x, eval e1 env, eval e2 env)
| LETREC (f, x, e1, e2) -> LETREC (f, x, e1, e2)
| PROC (x, e) -> PROC (x, eval e env)
| CALL (e1, e2) -> CALL (eval e1 env, eval e2 env)
in eval exp []


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
let rec find_var var env =
match env with
| [] -> []
| hd::tl -> if hd = var then find_var var tl else hd::(find_var var tl)
in
let rec eval lam =
match lam with
| V x -> [x]
| P(x,e) -> find_var x (eval e)
| C(e1,e2) -> (eval e1)@(eval e2)
in 
if (eval lam) = [] then true else false