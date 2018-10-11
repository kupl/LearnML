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

let rec adef
= fun e f def -> match e with 
CONST n -> CONST n
|VAR x -> if x = f then def else (VAR x)
|ADD (e1, e2) -> ADD ((adef e1 f def), (adef e2 f def))
|SUB (e1, e2) -> SUB ((adef e1 f def), (adef e2 f def))
|MUL (e1, e2) -> MUL ((adef e1 f def), (adef e2 f def))
|DIV (e1, e2) -> DIV ((adef e1 f def), (adef e2 f def))
|ISZERO (e) -> ISZERO (adef e f def)
|READ -> READ
|IF (e1, e2,e3) -> IF((adef e1 f def), (adef e2 f def), (adef e3 f def))
|LET (x, e1, e2) -> ( LET (x,(adef e1 f def), (adef e2 f def)))
|LETREC (x1, x2, e1, e2) -> LETREC(x1, x2, (adef e1 f def), (adef e2 f def))
|PROC (x, e) -> PROC (x, (adef e f def))
|CALL (e1,e2) -> CALL ((adef e1 f def), (adef e2 f def))

let rec isin
= fun e f -> match e with 
CONST n -> false
|VAR x -> if x = f then true else false
|ADD (e1,e2) -> (
    let x1 = isin e1 f in 
    let x2 = isin e2 f in
    if x1 || x2 then true else false
    )
|SUB (e1, e2) -> (
    let x1 = isin e1 f in
    let x2 = isin e2 f in
    if x1 || x2 then true else false
    )
|MUL (e1, e2) -> (
    let x1 = isin e1 f in 
    let x2 = isin e2 f in 
    if x1 || x2 then true else false
    )
|DIV (e1, e2) -> (
    let x1 = isin e1 f in
    let x2 = isin e2 f in 
    if x1 || x2 then true else false
    )
|ISZERO (e) -> isin e f
|READ -> false
|IF (e1, e2, e3) -> (
    let x1 = isin e1 f in
    let x2 = isin e2 f in
    let x3 = isin e3 f in
    if (x1 || x2) || x3 then true else false)
|LET (x, e1, e2) -> (
    let x1 = isin e1 f in
    let x2 = isin e2 f in
    if x1 || x2 then true else false)
|LETREC (x1, x2, e1, e2) -> (
    let y1 = isin e1 f in
    let y2 = isin e2 f in 
    if y1 || y2 then true else false
    )
|PROC (x, e1) -> (
    isin e1 f)
|CALL (e1, e2) -> (
    let x1 = isin e1 f in
    let x2 = isin e2 f in
    if x1 || x2 then true else false )


let rec expand : exp -> exp 
= fun exp -> match exp with 
CONST n -> CONST n
|VAR x -> VAR x
|ADD (e1, e2) -> ADD ((expand e1), (expand e2))
|SUB (e1, e2) -> SUB ((expand e1), (expand e2))
|MUL (e1, e2) -> MUL ((expand e1), (expand e2))
|DIV (e1, e2) -> DIV ((expand e1), (expand e2))
|ISZERO (e1)  -> ISZERO (expand e1)
|READ -> READ
|IF (e1, e2, e3) -> IF((expand e1), (expand e2), (expand e3))
|LET (x, e1, e2) -> (
    let exp1 = (expand e1) in 
    let exp2 = (expand e2) in
     if (isin exp2 x) then 
     (adef exp2 x exp1)
     else LET (x, exp1, exp2))
|LETREC (x1, x2, e1, e2) -> LETREC (x1, x2, (expand e1), (expand e2))
|PROC (x, e1) -> PROC (x, (expand e1))
|CALL (e1, e2) -> CALL (( expand e1), (expand e2))


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> true

let rec lambda ex l = 
match ex with 
|V(x) -> if List.mem x l then true else false
|P(x,e) -> lambda e(l@[x])
|C(e1, e2) -> (lambda e1 l) && (lambda e2 l)

let check ex =
lambda ex []
