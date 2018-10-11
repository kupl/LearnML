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

let rec apply_let exp l 
= match exp with
  	| CONST n -> exp
  	| VAR v -> (match l with 
			| (var1, let_exp) -> if (var1 = v) then let_exp else exp
			| _ -> exp)
  	| ADD (exp1, exp2) -> ADD (apply_let exp1 l, apply_let exp2 l)
  	| SUB (exp1, exp2) -> SUB (apply_let exp1 l, apply_let exp2 l)
  	| MUL (exp1, exp2) -> MUL (apply_let exp1 l, apply_let exp2 l)
  	| DIV (exp1, exp2) -> DIV (apply_let exp1 l, apply_let exp2 l)
  	| ISZERO exp1 -> ISZERO (apply_let exp1 l)
  	| READ -> exp
  	| IF (exp1, exp2, exp3) -> IF (apply_let exp1 l, apply_let exp2 l, apply_let exp3 l)
  	| LET (var1, exp1, exp2) -> LET (var1, apply_let exp1 l, apply_let exp2 l)
  	| LETREC (var1, var2, exp1, exp2) -> LETREC (var1, var2, apply_let exp1 l, apply_let exp2 l)
  	| PROC (var1, exp1) -> PROC (var1, apply_let exp1 l)
  	| CALL (exp1, exp2) -> CALL (apply_let exp1 l, apply_let exp2 l)

let rec expand : exp -> exp 
= fun exp -> match exp with
	| CONST n -> exp
  	| VAR v -> exp
  	| ADD (exp1, exp2) -> ADD (expand exp1, expand exp2)
  	| SUB (exp1, exp2) -> SUB (expand exp1, expand exp2)
  	| MUL (exp1, exp2) -> MUL (expand exp1, expand exp2)
 	| DIV (exp1, exp2) -> DIV (expand exp1, expand exp2)
  	| ISZERO exp1 -> ISZERO (expand exp1)
  	| READ -> exp
  	| IF (exp1, exp2, exp3) -> IF (expand exp1, expand exp2, expand exp3)
  	| LET (var1, exp1, exp2) -> let newexp = apply_let exp2 (var1, exp1) in if(newexp = exp2) then LET (var1, expand exp1, expand exp2) else expand newexp
  	| LETREC (var1, var2, exp1, exp2) -> LETREC (var1, var2, expand exp1, expand exp2)
  	| PROC (var1, exp1) -> PROC (var1, expand exp1)
  	| CALL (exp1, exp2) -> CALL (expand exp1, expand exp2)


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check_occurence v l
= match l with
	| hd::tl -> if (hd = v) then true else check_occurence v tl
	| [] -> false

let rec check_form  : lambda -> var list -> bool
= fun lam l -> match lam with
	| P (v, lambda1) -> check_form lambda1 ([v]@l)
	| V v -> check_occurence v l
	| C (lambda1, lambda2) -> (check_form lambda1 l) && (check_form lambda2 l)
	| _ -> false

let rec check : lambda -> bool
= fun lam -> check_form lam []


