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
let rec epd e l = 
match e with
| CONST n -> CONST n

| VAR x -> let rec search lst = 
match lst with
| [] -> VAR x
| hd::tl -> (match hd with (var,v) -> if x = var then v else search tl)
in search l

| ADD (e1,e2) -> ADD (epd e1 l, epd e2 l)
| SUB (e1,e2) -> SUB (epd e1 l, epd e2 l)
| MUL (e1,e2) -> MUL (epd e1 l, epd e2 l)
| DIV (e1,e2) -> DIV (epd e1 l, epd e2 l)

| ISZERO e1 -> ISZERO (epd e1 l)

| READ -> READ

| IF (e1, e2, e3) -> IF (epd e1 l, epd e2 l, epd e3 l)

| LET (x, e1, e2) -> let rec find ce cx =
match ce with
| CONST n -> false
| VAR x -> if x = cx then true else false
| ADD (e1,e2) -> (find e1 cx) || (find e2 cx)
| SUB (e1,e2) -> (find e1 cx) || (find e2 cx)
| MUL (e1,e2) -> (find e1 cx) || (find e2 cx)
| DIV (e1,e2) -> (find e1 cx) || (find e2 cx)
| ISZERO e1 -> (find e1 cx)
| READ -> false
| IF (e1, e2, e3) -> (find e1 cx) || (find e2 cx) || (find e3 cx)
| LET (x, e1, e2) -> if x = cx then false else (find e1 cx) || (find e2 cx) 
| LETREC (f,x,e1,e2) -> if f = cx then false else if x = cx then (find e2 cx) else (find e1 cx) || (find e2 cx) 
| PROC (x, e1) -> if x = cx then false else (find e1 cx)
| CALL (e1, e2) ->  (find e1 cx) || (find e2 cx)
in if (find e2 x) then (epd e2 ((x, epd e1 l)::l)) else LET (x, epd e1 l, epd e2 l)

| LETREC (f,x,e1,e2) -> LETREC (f,x, epd e1 ((x, VAR x)::l), epd e2 l)

| PROC (x, e1) -> PROC (x, epd e1 ((x, VAR x)::l))

| CALL (e1, e2) ->  CALL (epd e1 l, epd e2 l)
in epd exp []

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec che lam l = match lam with
| V x -> let rec search lst = 
match lst with
| [] -> false
| hd::tl -> if hd = x then true else search tl
in search l
| P (x,ld) -> che ld (x::l)
| C (ld1, ld2) -> (che ld1 l) && (che ld2 l)
in che lam []
