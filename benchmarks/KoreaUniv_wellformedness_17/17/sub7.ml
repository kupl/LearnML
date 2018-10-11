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

type let_env = (var * exp) list

let rec apply: let_env -> var -> exp
= fun env x ->
	match env with
	|[] -> VAR x 
	|hd::tl -> 
		let (var, exp) = hd in
			if var = x then exp else apply tl x

let rec check_exp: var -> exp -> bool
= fun x exp ->
	match exp with
	| VAR x -> true
	| ADD (e1, e2) -> check_exp x e1 || check_exp x e2
	| SUB (e1, e2) -> check_exp x e1 || check_exp x e2
	| MUL (e1, e2) -> check_exp x e1 || check_exp x e2
	| DIV (e1, e2) -> check_exp x e1 || check_exp x e2
	| ISZERO e -> check_exp x e
	| IF (e1, e2, e3) -> check_exp x e1 || check_exp x e2 || check_exp x e3
	| LET (x, e1, e2) -> check_exp x e1 || check_exp x e2
	| LETREC (f, x, e1, e2) -> check_exp x e1 || check_exp x e2
	| PROC (x, e) -> check_exp x e
	| CALL (e1, e2) -> check_exp x e1 || check_exp x e2
	| _ -> false

let rec replace: let_env -> exp -> exp
= fun env exp ->
	match exp with
	| VAR x -> apply env x
	| ADD (e1, e2) -> ADD ((replace env e1), (replace env e2))
	| SUB (e1, e2) -> SUB ((replace env e1), (replace env e2))
	| MUL (e1, e2) -> MUL ((replace env e1), (replace env e2))
	| DIV (e1, e2) -> DIV ((replace env e1), (replace env e2))
	| ISZERO e -> ISZERO (replace env e)
	| IF (e1, e2, e3) -> IF ((replace env e1), (replace env e2), (replace env e3))
	| LET (x, e1, e2) -> 
		let replaced_e2 = replace [] e2 in
			if check_exp x replaced_e2 then
				let nenv = (x, replace env e1)::[] in replace nenv replaced_e2
		else LET (x, e1, (replace env replaced_e2))
	| LETREC (f, x, e1, e2) -> LETREC (f, x, (replace env e1), (replace env e2)) (* is it right? *)
	| PROC (x, e) -> PROC (x, (replace env e))
	| CALL (e1, e2) -> CALL ((replace env e1), (replace env e2))
	| _ -> exp

let rec expand : exp -> exp 
= fun exp -> replace [] exp

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
type var_env = string list

let rec isbound: var_env -> string -> bool
= fun env str ->
	match env with
	|[] -> false
	|hd::tl -> if hd = str then true else isbound tl str

let rec check_lam
= fun env lam ->
	match lam with
	| V x -> if isbound env x then true else false
	| P (x, e) -> let nenv = x::env in check_lam nenv e
	| C (e1, e2) -> check_lam env e1 && check_lam env e2

let rec check : lambda -> bool
= fun lam -> check_lam [] lam
