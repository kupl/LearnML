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

let rec apply = fun var lst -> match lst with
                               | (x, y)::t -> if x = var then y else apply var t
                               | [] -> VAR var

(*added datatype. stores pairs of a variable and corresponding expression*)
type var_env = (var * exp) list

(*added function. returns 1 if the variable is shown in the expression, 0 otherwise*)
let rec bound_var : exp -> var -> int
= fun exp var -> match exp with
		  | CONST n -> 0
		  | VAR v -> if v = var then 1 else 0
		  | ADD (exp1, exp2) -> bound_var exp1 var lor bound_var exp2 var
		  | SUB (exp1, exp2) -> bound_var exp1 var lor bound_var exp2 var
		  | MUL (exp1, exp2) -> bound_var exp1 var lor bound_var exp2 var
		  | DIV (exp1, exp2) -> bound_var exp1 var lor bound_var exp2 var
		  | ISZERO exp -> bound_var exp var
		  | READ -> 0 
		  | IF (exp1, exp2, exp3) -> bound_var exp1 var lor bound_var exp2 var lor bound_var exp3 var
		  | LET (var, exp1, exp2) -> bound_var exp2 var
		  | LETREC (var1, var2, exp1, exp2) -> bound_var exp1 var lor bound_var exp2 var
		  | PROC (var, exp) -> bound_var exp var
		  | CALL (exp1, exp2) -> bound_var exp1 var lor bound_var exp2 var

(*added function. returns let-substituted form*)
let rec subst : exp -> var_env -> exp
= fun exp env -> match exp with
		  | CONST n -> CONST n
		  | VAR var -> apply var env
		  | ADD (exp1, exp2) -> ADD (subst exp1 env, subst exp2 env)
		  | SUB (exp1, exp2) -> SUB (subst exp1 env, subst exp2 env)
		  | MUL (exp1, exp2) -> MUL (subst exp1 env, subst exp2 env)
		  | DIV (exp1, exp2) -> DIV (subst exp1 env, subst exp2 env)
		  | ISZERO exp -> ISZERO (subst exp env)
		  | READ -> READ
		  | IF (exp1, exp2, exp3) -> IF (subst exp1 env, subst exp2 env, subst exp3 env)
		  | LET (var, exp1, exp2) -> if (bound_var exp2 var = 1) then subst exp2 ((var, exp1)::env) 
                                             else LET (var, exp1, (subst exp2 env))
		  | LETREC (var1, var2, exp1, exp2) -> LETREC (var1, var2, subst exp1 env, subst exp2 env)
		  | PROC (var, exp) -> PROC (var, subst exp env)
		  | CALL (exp1, exp2) -> CALL(subst exp1 env, subst exp2 env)

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> subst exp []


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda

(*added function. returns true if given variable is bound, false otherwise*)
let rec apply = fun var env -> match env with
                               | h::t -> if var=h then true else apply var t
                               | [] -> false

(*added function. checks all variables in the body of the given lambda expression whether it is bound or not*)
let rec bound_var = fun l env -> match l with
                                 | V var -> apply var env
                                 | P (var, lambda) -> bound_var lambda (var::env)
                                 | C (lambda1, lambda2) -> (bound_var lambda1 env) && (bound_var lambda2 env)

let rec check : lambda -> bool
= fun lam -> bound_var lam []
