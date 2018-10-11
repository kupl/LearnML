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
let rec explst : (var * exp) list -> exp -> exp
= fun lst exp -> match exp with
| CONST n -> CONST n
| VAR x -> let rec explst_var : (var * exp) list -> var -> exp
= fun lst x -> (match lst with
| [] -> VAR x
| hd::tl -> (match hd with
  | (var, exp) -> if (var = x) then (explst tl exp) else explst_var tl x
)) in explst_var lst x
| ADD (e1, e2) -> ADD (explst lst e1, explst lst e2)
| SUB (e1, e2) -> SUB (explst lst e1, explst lst e2)
| MUL (e1, e2) -> MUL (explst lst e1, explst lst e2)
| DIV (e1, e2) -> DIV (explst lst e1, explst lst e2)
| ISZERO e -> ISZERO (explst lst e)
| READ -> READ
| IF (e1, e2, e3) -> IF (explst lst e1, explst lst e2, explst lst e3)
| LET (x, e1, e2) -> explst ((x,e1)::lst) e2
| LETREC (f, x, e1, e2) -> (match lst with
  | [] -> LETREC (f, x, e1, explst lst e2)
  | hd::tl -> (match hd with
    | (var, exp) -> if (var = x) then (LETREC (f, x, explst tl e1, explst lst e2))
      else (LETREC (f, x, explst lst e1, explst lst e2))
    )
)
| PROC (v, e) -> PROC (v, e)
| CALL (e1, e2) -> CALL (explst lst e1, explst lst e2)

let rec checklet : (var * exp) list -> exp -> bool
= fun lst exp -> match exp with
| CONST n -> false
| VAR x -> let rec checklet_var : (var * exp) list -> var -> bool
= fun lst x -> (match lst with
| [] -> false
| hd::tl -> (match hd with
  | (var, exp) -> if (var = x) then true else checklet_var tl x
)) in checklet_var lst x
| ADD (e1, e2) -> checklet lst e1 || checklet lst e2
| SUB (e1, e2) -> checklet lst e1 || checklet lst e2
| MUL (e1, e2) -> checklet lst e1 || checklet lst e2
| DIV (e1, e2) -> checklet lst e1 || checklet lst e2
| ISZERO e -> checklet lst e
| READ -> false
| IF (e1, e2, e3) -> checklet lst e1 || checklet lst e2 || checklet lst e3
| LET (x, e1, e2) -> checklet lst (explst [(x,e1)] e2)
| LETREC (f, x, e1, e2) -> (match lst with
  | [] -> checklet lst e2
  | hd::tl -> (match hd with
    | (var, exp) -> if (var = x) then (checklet tl e1 || checklet lst e2) else (checklet lst e1 || checklet lst e2)
)) 
| PROC (v, e) -> false
| CALL (e1, e2) -> checklet lst e1 || checklet lst e2


let rec expand : exp -> exp 
= fun exp -> match exp with
  | CONST n -> CONST n
  | VAR x -> VAR x
  | ADD (e1, e2) -> ADD (expand e1, expand e2)
  | SUB (e1, e2) -> SUB (expand e1, expand e2)
  | MUL (e1, e2) -> MUL (expand e1, expand e2)
  | DIV (e1, e2) -> DIV (expand e1, expand e2)
  | ISZERO e -> ISZERO (expand e)
  | READ -> READ
  | IF (e1, e2, e3) -> IF (expand e1, expand e2, expand e3)
  | LET (x, e1, e2) -> if (checklet [(x, e1)] e2) 
        then explst [(x, e1)] e2 else LET(x, e1, expand e2)
  | LETREC (f, x, e1, e2) -> LETREC (f, x, e1, e2)
  | PROC (v, e) -> PROC (v, expand e)
  | CALL (e1, e2) -> CALL (expand e1, expand e2)


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec complst : var list -> var -> bool
= fun lst v -> match lst with
|[] -> false
|hd::tl -> if (hd = v) then true else complst tl v

let rec checklst : var list -> lambda -> bool
= fun lst lam -> match lam with
| V x -> complst lst x
| P (x, l) -> checklst (x::lst) l
| C (l1, l2) -> checklst lst l1 && checklst lst l2

let rec check : lambda -> bool
= fun lam -> match lam with
| V x -> false
| P (x, l) -> checklst [x] l
| C (l1, l2) -> (check l1) && (check l2)