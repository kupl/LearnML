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

(*define datatypses*)
type expenv = (var * exp) list
let empty_env = []
let extend_env (x, e) exe = (x,e)::exe
let rec apply_env exe x =
	match exe with
	| [] -> raise (Failure (x ^ " is unbound in Env"))
	| (y, e) :: tl -> if x = y then e else apply_env tl x

let rec check_var : exp -> var -> bool
= fun exp var ->
match exp with
|CONST n -> false
|VAR x ->
	if x = var then true else false
|ADD (e1, e2) ->
	(check_var e1 var) || (check_var e2 var)
|SUB (e1, e2) ->
	(check_var e1 var) || (check_var e2 var)
|MUL (e1, e2) ->
	(check_var e1 var) || (check_var e2 var)
|DIV (e1, e2) ->
	(check_var e1 var) || (check_var e2 var)
|READ -> false
|ISZERO e -> check_var e var
|IF (e, e1, e2) -> 
	(check_var e var) || (check_var e1 var) || (check_var e2 var)
|LET (x, e1, e2) ->
	(check_var e1 var) || (check_var e2 var)
|LETREC (f, x, e1, e2) ->
	(check_var e1 var) || (check_var e2 var)
|PROC (x, e) -> check_var e var
|CALL (e1, e2) -> 
	(check_var e1 var) || (check_var e2 var)

let rec apply_let : exp -> expenv -> var -> exp
= fun exp env var ->
match exp with
|CONST n -> CONST n
|VAR x -> apply_env env x
|ADD (e1, e2) -> 
	let e1' = apply_let e1 env var in
	let e2' = apply_let e2 env var in
		ADD(e1', e2')
|SUB (e1, e2) ->
   let e1' = apply_let e1 env var in
   let e2' = apply_let e2 env var in
     SUB(e1', e2')
|MUL (e1, e2) ->
   let e1' = apply_let e1 env var in
   let e2' = apply_let e2 env var in
     MUL(e1', e2')
|DIV (e1, e2) ->
   let e1' = apply_let e1 env var in
   let e2' = apply_let e2 env var in
     DIV(e1', e2')
|READ -> READ
|ISZERO e -> 
	let e' = apply_let e env var in
		ISZERO e'
|IF (e, e1, e2) ->
	let e' = apply_let e env var in
	let e1' = apply_let e1 env var in
	let e2' = apply_let e2 env var in
		IF ( e', e1', e2')
|LET (x, e1, e2) ->
	let e1' = apply_let e1 env var in
	let e2' = apply_let e2 env var in
  if (check_var e2' x) = true 
		then let letenv = extend_env (x, e1') [] in
			apply_let e2' letenv x
		else LET (x, e1', e2')
|LETREC (f, x, e1, e2) ->
	let e1' = apply_let e1 env var in
	let e2' = apply_let e2 env var in
		LETREC (f, x, e1', e2')
|PROC (x, e) ->
	let e' = apply_let e env var in
		PROC(x, e')
|CALL (e1, e2) -> 
	let e1' = apply_let e1 env var in
	let e2' = apply_let e2 env var in
		CALL (e1', e2')

let rec expand : exp -> exp 
= fun exp ->
match exp with 
| LET ( x, e1, e2) -> 
	if (check_var e2 x) = true 
		then let letenv = extend_env (x, e1) [] in
			apply_let e2 letenv x	
		else exp
| _ -> exp


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type lambda_env = var list

let rec extend_lenv x lenv = x :: lenv

let rec l_check : lambda -> lambda_env -> bool
= fun lam lenv ->
match lam with
| V x -> 
	(match lenv with
	|[] -> false
	|hd::tl -> if x = hd then true else l_check lam tl)
| P (x, lam) -> 
	let lenv = extend_lenv x lenv in
		l_check lam lenv
| C (l1, l2) ->	
	(l_check l1 lenv) && (l_check l2 lenv)



let rec check : lambda -> bool
= fun lam -> l_check lam []


