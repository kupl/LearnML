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

let rec findvarandreplace : var -> exp -> exp -> exp
= fun v ve re -> 
  match re with 
  | CONST n -> CONST n
  | VAR v1 -> if v1 = v then ve else VAR v1
  | ADD (e1, e2) -> ADD (findvarandreplace v ve e1, findvarandreplace v ve e2)
  | SUB (e1, e2) -> SUB (findvarandreplace v ve e1, findvarandreplace v ve e2)
  | MUL (e1, e2) -> MUL (findvarandreplace v ve e1, findvarandreplace v ve e2)
  | DIV (e1, e2) -> DIV (findvarandreplace v ve e1, findvarandreplace v ve e2)
  | READ ->  READ
  | ISZERO e1 -> ISZERO (findvarandreplace v ve e1)
  | IF (ef, e1, e2) -> IF (findvarandreplace v ve ef, findvarandreplace v ve e1, findvarandreplace v ve e2)
  | LET (v1, e1, e2) -> if v1 = v then LET (v1, e1, e2) else LET (v1, findvarandreplace v ve e1, findvarandreplace v ve e2)
  | LETREC (f, x, e1, e2) -> if f = v then LETREC(f, x, e1, e2) else LETREC(f, x, findvarandreplace v ve e1, findvarandreplace v ve e2) 
  | PROC (v1, e1) -> PROC (v1, findvarandreplace v ve e1)
  | CALL (e1, e2) -> CALL (findvarandreplace v ve e1, findvarandreplace v ve e2)

let rec expand : exp -> exp 
= fun e -> 
  match e with
  | CONST n -> CONST n
  | VAR v -> VAR v
  | ADD (e1, e2) -> ADD (expand e1, expand e2)
  | SUB (e1, e2) -> SUB (expand e1, expand e2)
  | MUL (e1, e2) -> MUL (expand e1, expand e2)
  | DIV (e1, e2) -> DIV (expand e1, expand e2)
  | READ ->  READ
  | ISZERO e1 -> ISZERO (expand e1)
  | IF (ef, e1, e2) -> IF (expand ef, expand e1, expand e2)
  | LET (v, e1, e2) -> (
      let newe = findvarandreplace v e1 e2 in 
      if (newe = e2) then LET (v, expand e1, expand e2) else expand newe
    )
  | LETREC (f, x, e1, e2) -> CALL ((expand (LET(f, PROC (x, VAR x), e1))),(expand (LET(f, PROC (x, VAR x),e2)))) 
  | PROC (v, e1) -> PROC (v, expand e1)
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
  let rec removevar : var -> (var list) -> (var list)
  = fun v l -> 
    match l with 
    | [] -> []
    | h::t -> if h = v then (removevar v t) else [h] @ (removevar v t)
  in
  let rec checklist : lambda ->  (var list)
  = fun lm ->
    match lm with 
    | V v -> [v]
    | P (v, l) -> (removevar v (checklist l))
    | C (l1, l2) -> ((checklist l1) @ (checklist l2))
  in
  match (checklist lam) with
  | [] -> true
  | _ -> false
