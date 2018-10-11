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

type expenv = (var * exp) list
let empty_env = []
let extend_env (x, e) expev = (x,e)::expev
let rec apply_env expev x =
	begin
		match expev with
		| [] -> raise (Failure (x ^ " is unbound in env"))
		| (y, e)::tl -> if x = y then e else apply_env tl x
	end

let rec checkvar : exp -> var -> bool
= fun exp var ->
begin
	match exp with
	| CONST n -> false
	| VAR v -> if v = var then true else false
	| ADD (e1, e2) -> (checkvar e1 var) || (checkvar e2 var)
	| SUB (e1, e2) -> (checkvar e1 var) || (checkvar e2 var)
	| MUL (e1, e2) -> (checkvar e1 var) || (checkvar e2 var)
	| DIV (e1, e2) -> (checkvar e1 var) || (checkvar e2 var)
	| ISZERO e1 -> checkvar e1 var
	| READ -> false
	| IF (e1, e2, e3) -> (checkvar e1 var) || (checkvar e2 var) || (checkvar e3 var)
	| LET (v, e1, e2) -> (checkvar e1 var) || (checkvar e2 var)
	| LETREC (f, v, e1, e2) -> (checkvar e1 var) || (checkvar e2 var)
	| PROC (v, e1) -> checkvar e1 var
	| CALL (e1, e2) -> (checkvar e1 var) || (checkvar e2 var)
end

let rec let_apply : exp -> expenv -> var -> exp
= fun exp env var ->
begin
	match exp with
	| CONST n -> CONST n
	| VAR v -> apply_env env v
	| ADD (e1, e2) -> 
		let e_1 = let_apply e1 env var in
		let e_2 = let_apply e2 env var in ADD(e_1, e_2)
	| SUB (e1, e2) -> 
		let e_1 = let_apply e1 env var in
		let e_2 = let_apply e2 env var in SUB(e_1, e_2)
	| MUL (e1, e2) ->
		let e_1 = let_apply e1 env var in
		let e_2 = let_apply e2 env var in MUL(e_1, e_2)
	| DIV (e1, e2) ->
		let e_1 = let_apply e1 env var in
		let e_2 = let_apply e2 env var in DIV(e_1, e_2)
	| ISZERO e1 -> let e_1 = let_apply e1 env var in ISZERO e_1
	| READ -> READ
	| IF (e1, e2, e3) ->
		let e_1 = let_apply e1 env var in
		let e_2 = let_apply e2 env var in
		let e_3 = let_apply e3 env var in IF(e_1, e_2, e_3)
	| LET (v, e1, e2) ->
		let e_1 = let_apply e1 env var in
		let e_2 = let_apply e2 env var in
		if (checkvar e_2 v) = true 
			then let l_env = extend_env (v, e_1) [] in let_apply e_2 l_env v
		else LET (v, e_1, e_2)
	| LETREC (f, v, e1, e2) ->
		let e_1 = let_apply e1 env var in
		let e_2 = let_apply e2 env var in LETREC (f, v, e_1, e_2)
	| PROC (v, e1) -> let e_1 = let_apply e1 env var in PROC (v, e_1)
	| CALL (e1, e2) -> let e_1 = let_apply e1 env var in
						let e_2 = let_apply e2 env var in CALL (e_1, e_2)
end
(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> 
begin
	match exp with
	| LET (v, e1, e2) -> if (checkvar e2 v) = true then let l_env = extend_env (v, e1) [] in
							let_apply e2 l_env v
						 else exp
	| _ -> exp
end


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
type env = var list

let rec check_2 : env -> lambda -> bool
= fun env lamb ->
begin
	match lamb with
	|V v -> 
	begin
		match env with
		|[] -> false
		|hd::tl -> if v = hd then true else check_2 tl lamb
	end
	|P (v, lamb') -> check_2 ([v]@env) lamb'
	|C (lamb_1, lamb_2) -> (check_2 env lamb_1) && (check_2 env lamb_2)
end

let rec check : lambda -> bool
= fun lam -> check_2 [] lam



