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
type env = (var * exp) list

let empty_env = []
(*let extend_env (x,v) e = (x,v)::e*)

let rec update_env : env -> var -> exp -> env
= fun env x e1 -> match env with
		| [] -> [(x,e1)]
		| (v,e)::tl -> if v = x then [(x,e1)]@tl else [(v,e)]@update_env tl x e1

(*let rec find_env*) 
(*
if find_env env (x,e1) then env else apply_env env (x,e1)
*)
let rec apply_env : env -> exp -> exp
= fun env exp -> match exp with
		| CONST n -> CONST n
		| VAR x -> (match env with 
			 | [] -> VAR x
			 | (v,e)::tl -> if x=v then e else apply_env tl exp)
		| ADD (e1,e2) -> ADD (apply_env env e1, apply_env env e2)
		| SUB (e1,e2) -> SUB (apply_env env e1, apply_env env e2)
		| MUL (e1,e2) -> MUL (apply_env env e1, apply_env env e2)
		| DIV (e1,e2) -> DIV (apply_env env e1, apply_env env e2)			
		| ISZERO e -> ISZERO (apply_env env e)		
		| READ -> CONST (read_int())		
		| IF (e1,e2,e3) -> IF (apply_env env e1, apply_env env e2, apply_env env e3)
		| LET (x,e1,e2) -> let after_exp = apply_env (update_env env x e1) e2 in
					if after_exp = e2 then LET (x,e1,e2) else after_exp

(*let after_exp = apply_env (update_env env x e1) e2 in
					if after_exp = e2 then LET (x,e1,e2) else after_exp
*)				  (*
				   let after_env = update_env env x e1 in
			    	   let after_exp = apply_env after_env e2 in
				 	(match e2 with
					| LET (x,e3,e4) -> let after_exp2 = apply_env (update_env after_env x e3) e4 in
								if after_exp2 = e4 then LET (x,e3,e4) else apply_env (LET (x,e1,after_exp2)) (*else need apply??*) 
					| _ ->  if after_exp = e2 then LET (x,e1,e2) else after_exp (*apply_env*)
					)*)(*
*)
			(* in case e2 is let, we have to modify*)
		| LETREC (f,x,e1,e2) -> LETREC (f,x,apply_env env e1, apply_env env e2) 		
		| PROC (x,e) -> PROC (x, apply_env env e)		
		| CALL (e1,e2) -> CALL (apply_env env e1, apply_env env e2)
		
(*
expand (LET ("f", PROC ("x", SUB (VAR "x",CONST 11)), CALL (VAR "f", CALL (VAR "f", CONST 77))));;
expand (LET ("x", CONST 1, (LET ("x",CONST 2, VAR "x"))));;
expand (LET ("x", CONST 1, (LET ("x",CONST 2, (LET ("x", CONST 3, VAR "x"))))));;
let rec delete_let : env -> exp -> exp
= fun env exp -> match exp with
		| CONST n -> CONST n
		| VAR x -> VAR x
		| ADD (e1,e2) -> ADD (delete_let env e1,delete_let env e2)
		| SUB (e1,e2) -> SUB (delete_let env e1,delete_let env e2)
		| MUL (e1,e2) -> MUL (delete_let env e1,delete_let env e2)
		| DIV (e1,e2) -> DIV (delete_let env e1,delete_let env e2)
		| ISZERO e -> ISZERO (delete_let env e)
		| READ -> CONST (read_int()) (*right??*)
		| IF (e1,e2,e3) -> IF (delete_let env e1,delete_let env e2,delete_let env e3)
		| LET (x,e1,e2) -> match env with
				| [] -> (match e2 with
					| VAR x -> e1
					| _ -> (*delete_let*)find_env (update_env env x e1) e2)
				| (v,e)::tl -> if x = v then update_env  e2
							else delete_let tl e2

if apply_env (update_env env x e1) e2 = e2 then LET(x,e1,e2)
		
		| CALL (e1,e2) -> CALL (delete_let env e1,delete_let env e2)
		(*
*)
if apply_env (update_env x e1) e2 = e then 




(let rec apply_env : env -> exp -> exp  
				= fun env exp2 -> match env with
						| [] -> LET (x,e1 apply_env env e2)
						| (v,e)::tl -> (match exp2 with
									apply_let (update_env env x e1) exp2)
								| VAR x -> if x = v then (*apply_let env*) e else apply_env tl exp2
								| _ -> LET (x,e1,e2)(*right??*))
				(*need to always pass extend env*)
(*(apply_env (update_env env x e1) e2)*)

		| CALL (e1,e2) -> CALL (apply_let env e1,apply_let env e2)
		| PROC (x,e) -> PROC (x, apply_let env e)(*x fix needed??*)
		(*
 
| LETREC (f,x,e1,e2) -> 
*)
(*	*)*)

let rec expand : exp -> exp 
= fun exp -> apply_env [] exp

			
(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type variables = var list (*necessary?? var?? *)

let rec findVar : var list -> lambda -> bool
= fun vlist lam -> match lam with
		| V (x) -> (match vlist with
			| [] -> false 
			| hd::tl -> if hd = x then true else findVar tl lam)
		| P (v,l) -> findVar (v::vlist) l 
		| C (l1,l2) -> (findVar vlist l1)&&(findVar vlist l2)
	
let rec check : lambda -> bool
= fun lam -> findVar [] lam

(* 
check (P ("a", V "a"));;
check (P ("a", P ("a", V "a")));;
check (P ("a", P ("b", C (V "a", V "b"))));;
check (P ("a", C (V "a", P ("b", V "a"))));;
check (P ("a", V "b"));;
check (P ("a", C (V "a", P ("b", V "c"))));;
check (P ("a", P ("b", C (V "a", V "c"))));;
*)
