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

let rec findvar : var -> exp -> bool
= fun s e ->
match e with
| CONST n -> false
| VAR x -> if (x=s) then true else false
| ADD (e1,e2) -> (findvar s e1) || (findvar s e2)
| SUB (e1,e2) -> (findvar s e1) || (findvar s e2)
| MUL (e1,e2) -> (findvar s e1) || (findvar s e2)
| DIV (e1,e2) -> (findvar s e1) || (findvar s e2)
| READ -> false
| ISZERO e1 -> (findvar s e1)
| IF (e1,e2,e3) -> (findvar s e1) || (findvar s e2) || (findvar s e3)
| LETREC (f,x,e1,e2) -> (findvar s e1) || (findvar s e2)
| PROC (x,e1) -> (findvar s e1)
| CALL (e1,e2) -> (findvar s e1) || (findvar s e2)
| LET (x,e1,e2) -> (findvar s e1) || (findvar s e2)

let rec exchg : var -> exp -> exp -> exp
= fun s e1 e2 ->
match e2 with
| CONST n -> CONST n
| VAR x -> if (s=x) then e1 else (VAR x)
| ADD (e3,e4) -> ADD ((exchg s e1 e3),(exchg s e1 e4))
| SUB (e3,e4) -> SUB ((exchg s e1 e3),(exchg s e1 e4))
| MUL (e3,e4) -> MUL ((exchg s e1 e3),(exchg s e1 e4))
| DIV (e3,e4) -> DIV ((exchg s e1 e3),(exchg s e1 e4))
| READ -> READ
| ISZERO e3 -> ISZERO (exchg s e1 e3)
| IF (e3,e4,e5) -> IF ((exchg s e1 e3),(exchg s e1 e4),(exchg s e1 e5))
| LETREC (f,x,e3,e4) -> LETREC (f,x,(exchg s e1 e3),(exchg s e1 e4))
| PROC (x,e3) -> PROC (x,(exchg s e1 e3))
| CALL (e3,e4) -> CALL ((exchg s e1 e3),(exchg s e1 e4))
| LET (x,e3,e4) -> LET (x,(exchg s e1 e3),(exchg s e1 e4))

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun e ->
match e with
| LET (x,e1,e2) -> if (findvar x e2)=true then (expand (exchg x e1 e2))
                   else e
|_ -> e


(**********************)
(*   Problem 2        *)
(*********************)
type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
type env = var list

let extend_env x e = x::e
let rec apply_env e x =
  match e with
  | [] -> false
  | hd::tl -> if hd=x then true else apply_env tl x

let rec eval : lambda -> env -> bool
= fun l e ->
match l with
| V x -> if (apply_env e x)=true then true
         else false
| P (x,l1) -> eval l1 (extend_env x e)
| C (l1,l2) ->  (eval l1 e) && (eval l2 e)

let rec check : lambda -> bool
= fun lam -> (eval lam [])

