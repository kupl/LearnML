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

let rec findvar : var -> exp -> bool
= fun x ex -> match ex with
				| CONST n -> false
				| VAR v -> if v = x then true else false
				| ADD (e1,e2) -> (findvar x e1) || (findvar x e2)
				| SUB (e1,e2) -> (findvar x e1) || (findvar x e2)
				| MUL (e1,e2) -> (findvar x e1) || (findvar x e2)
				| DIV (e1,e2) -> (findvar x e1) || (findvar x e2)
				| READ -> false
				| ISZERO e1 -> findvar x e1
				| IF (e1,e2,e3) -> (findvar x e1) || (findvar x e2) || (findvar x e3)
				| LET (y,e1,e2) -> (findvar x (VAR y)) || (findvar x e1) || (findvar x e2)
				| LETREC (f,y,e1,e2) -> (findvar x (VAR f)) || (findvar x (VAR y)) || (findvar x e1) || (findvar x e2)
				| PROC (y,e1) -> (findvar x (VAR y)) || (findvar x e1)
				| CALL (e1,e2) -> (findvar x e1) || (findvar x e2)

let rec changevar : var -> exp -> exp -> exp
= fun x e ex-> match ex with
			| CONST n -> CONST n
			| VAR v -> if v = x then e else ex
			| ADD (e1,e2) -> ADD ((changevar x e e1), (changevar x e e2))
			| SUB (e1,e2) -> SUB ((changevar x e e1), (changevar x e e2))
			| MUL (e1,e2) -> MUL ((changevar x e e1), (changevar x e e2))
			| DIV (e1,e2) -> DIV ((changevar x e e1), (changevar x e e2))
			| READ -> READ
			| ISZERO e1 -> ISZERO (changevar x e e1)
			| IF (e1,e2,e3) -> IF ((changevar x e e1), (changevar x e e2), (changevar x e e3))
			| LET (y,e1,e2) -> LET (y, (changevar x e e1), (changevar x e e2))
			| LETREC (f,y,e1,e2) -> LETREC (f, y, (changevar x e e1), (changevar x e e2))
			| PROC (y,e1) -> PROC (y, (changevar x e e1))
			| CALL (e1,e2) -> CALL ((changevar x e e1), (changevar x e e2))

let rec expand : exp -> exp 
= fun exp -> match exp with
			|LET (x,a,b) -> if (findvar x b) then expand (changevar x a b)
							else LET (x,a,(expand b))
			|_ -> exp
							

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let extend v lis = v::lis

let rec find v lis = match lis with
					| [] -> false
					| hd::tl -> if hd = v then true else find v tl

let rec checkfree : lambda -> var list -> bool
= fun lam vlist -> match lam with
			| V x -> find x vlist
			| P (x,l) -> checkfree l (x::vlist)
			| C (l1,l2) -> (checkfree l1 vlist) && (checkfree l2 vlist)

let rec check : lambda -> bool
= fun lam -> match lam with
			| V x -> false
			| P (x,l) -> checkfree l (x::[])
			| C (l1,l2) -> (check l1) && (check l2)