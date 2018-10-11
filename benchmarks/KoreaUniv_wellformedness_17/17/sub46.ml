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
and def = (var * exp) list

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)

(* You can define datatypes and helper functions as necessary *)

let rec apply_def 
= fun e f de -> match e with
  CONST n -> CONST n
| VAR x -> if x = f then de else (VAR x)
| ADD (e1,e2) -> ADD ((apply_def e1 f de), (apply_def e2 f de))
| SUB (e1,e2) -> SUB ((apply_def e1 f de), (apply_def e2 f de))
| MUL (e1,e2) -> MUL ((apply_def e1 f de), (apply_def e2 f de))
| DIV (e1,e2) -> DIV ((apply_def e1 f de), (apply_def e2 f de))
| ISZERO (e1) -> ISZERO (apply_def e1 f de)
| READ -> READ
| IF (e1,e2,e3) -> IF ((apply_def e1 f de), (apply_def e2 f de), (apply_def e3 f de))
| LET (x,e1,e2) -> (
  LET (x, (apply_def e1 f de), (apply_def e2 f de))
)
| LETREC (x1,x2,e1,e2) -> LETREC (x1, x2, (apply_def e1 f de), (apply_def e2 f de))
| PROC (x,e1) -> PROC (x, (apply_def e1 f de))
| CALL (e1,e2) -> CALL ((apply_def e1 f de), (apply_def e2 f de))

let rec is_in
= fun e f -> match e with
  CONST n -> false
| VAR x -> if x = f then true else false
| ADD (e1,e2) -> (
  let b1 = is_in e1 f in
  let b2 = is_in e2 f in
  if b1 || b2 then true else false
)
| SUB (e1,e2) -> (
  let b1 = is_in e1 f in
  let b2 = is_in e2 f in
  if b1 || b2 then true else false
)
| MUL (e1,e2) -> (
  let b1 = is_in e1 f in
  let b2 = is_in e2 f in
  if b1 || b2 then true else false
)
| DIV (e1,e2) -> (
  let b1 = is_in e1 f in
  let b2 = is_in e2 f in
  if b1 || b2 then true else false
)
| ISZERO (e1) -> (
  is_in e1 f
)
| READ -> false
| IF (e1,e2,e3) -> (
  let b1 = is_in e1 f in
  let b2 = is_in e2 f in
  let b3 = is_in e3 f in
  if (b1 || b2) || b3 then true else false
)
| LET (x,e1,e2) -> (
  let b1 = is_in e1 f in
  let b2 = is_in e2 f in
  if b1 || b2 then true else false
)
| LETREC (x1,x2,e1,e2) -> (
  let b1 = is_in e1 f in
  let b2 = is_in e2 f in
  if b1 || b2 then true else false
)
| PROC (x,e1) -> (
  is_in e1 f
)
| CALL (e1,e2) -> (
  let b1 = is_in e1 f in
  let b2 = is_in e2 f in
  if b1 || b2 then true else false
)

let rec expand : exp -> exp 
= fun e -> match e with
  CONST n -> CONST n
| VAR x -> VAR x
| ADD (e1,e2) -> ADD ((expand e1), (expand e2))
| SUB (e1,e2) -> SUB ((expand e1), (expand e2))
| MUL (e1,e2) -> MUL ((expand e1), (expand e2))
| DIV (e1,e2) -> DIV ((expand e1), (expand e2))
| ISZERO (e1) -> ISZERO (expand e1)
| READ -> READ
| IF (e1,e2,e3) -> IF ((expand e1), (expand e2), (expand e3))
| LET (x,e1,e2) -> (
  let pre_e1 = (expand e1) in
  let pre_e2 = (expand e2) in
    if (is_in pre_e2 x) then  (
      apply_def pre_e2 x pre_e1
    ) else LET (x, pre_e1, pre_e2)
)
| LETREC (x1,x2,e1,e2) -> LETREC (x1, x2, (expand e1), (expand e2))
| PROC (x,e1) -> PROC (x, (expand e1))
| CALL (e1,e2) -> CALL ((expand e1), (expand e2))

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
and env = var list

let empty_env = []
let extend_env v e = v::e
let rec apply_env e x = 
  match e with
  | [] -> false
  | y::tl -> if x = y then true else apply_env tl x

let rec isclosed : lambda -> env -> bool
=fun lam curr_env ->
  match lam with
  V v -> apply_env curr_env v
| P (v,e) -> isclosed e (extend_env v curr_env)
| C (e1,e2) -> (
    let b1 = isclosed e1 curr_env in
    let b2 = isclosed e2 curr_env in
    if b1 && b2 then true else false
)

let rec check : lambda -> bool
= fun lam -> isclosed lam empty_env