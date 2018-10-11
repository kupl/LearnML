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
  let rec varexist e v =
    match e with
    | CONST n -> false
    | VAR x -> if x = v then true else false
    | ADD (e1, e2) -> (varexist e1 v) || (varexist e2 v)
    | SUB (e1, e2) -> (varexist e1 v) || (varexist e2 v)
    | MUL (e1, e2) -> (varexist e1 v) || (varexist e2 v)
    | DIV (e1, e2) -> (varexist e1 v) || (varexist e2 v)
    | ISZERO e1 -> varexist e1 v
    | READ -> false
    | IF (e1, e2, e3) -> (varexist e1 v) || (varexist e2 v) || (varexist e3 v)
    | LET (x, e1, e2) -> if x = v then varexist e1 v
                         else (varexist e1 v) || (varexist e2 v)
    | LETREC (f, x, e1, e2) -> if f = v then false
                               else if x = v then varexist e2 v
                               else (varexist e1 v) || (varexist e2 v)
    | PROC (x, e1) -> if x = v then false else varexist e1 v
    | CALL (e1, e2) -> (varexist e1 v) || (varexist e2 v) in
  let rec replace e v ex =
    match e with
    | CONST n -> CONST n
    | VAR x -> if x = v then ex else VAR x
    | ADD (e1, e2) -> ADD (replace e1 v ex, replace e2 v ex)
    | SUB (e1, e2) -> SUB (replace e1 v ex, replace e2 v ex)
    | MUL (e1, e2) -> MUL (replace e1 v ex, replace e2 v ex)
    | DIV (e1, e2) -> DIV (replace e1 v ex, replace e2 v ex)
    | ISZERO e1 -> ISZERO (replace e1 v ex)
    | READ -> READ
    | IF (e1, e2, e3) -> IF (replace e1 v ex, replace e2 v ex, replace e3 v ex)
    | LET (x, e1, e2) -> if x = v then LET (x, replace e1 v ex, e2)
                         else LET (x, replace e1 v ex, replace e2 v ex)
    | LETREC (f, x, e1, e2) -> if f = v then e
                               else if x = v then LETREC (f, x, e1, replace e2 v ex)
                               else LETREC (f, x, replace e1 v ex, replace e2 v ex)
    | PROC (x, e1) -> if x = v then e else PROC (x, replace e1 v ex)
    | CALL (e1, e2) -> CALL (replace e1 v ex, replace e2 v ex) in
  match exp with
  | CONST n -> CONST n
  | VAR x -> VAR x
  | ADD (e1, e2) -> ADD (expand e1, expand e2)
  | SUB (e1, e2) -> SUB (expand e1, expand e2)
  | MUL (e1, e2) -> MUL (expand e1, expand e2)
  | DIV (e1, e2) -> DIV (expand e1, expand e2)
  | ISZERO e1 -> ISZERO (expand e1)
  | READ -> READ
  | IF (e1, e2, e3) -> IF (expand e1, expand e2, expand e3)
  | LET (x, e1, e2) -> if varexist e2 x then expand (replace e2 x e1)
                       else LET (x, expand e1, expand e2)
  | LETREC (f, x, e1, e2) -> if varexist e1 f then LETREC (f, x, expand e1, expand e2)
                             else expand (LET (f, PROC (x, e1), e2))
  | PROC (x, e1) -> PROC (x, expand e1)
  | CALL (e1, e2) -> CALL (expand e1, expand e2)


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam ->
  let rec find x l =
    match l with
    | [] -> false
    | h::t -> if x = h then true else find x t in
  let rec check2 la l =
    match la with
    | V v -> find v l
    | P (v, la1) -> check2 la1 (v::l)
    | C (la1, la2) -> (check2 la1 l) && (check2 la2 l) in
  check2 lam []