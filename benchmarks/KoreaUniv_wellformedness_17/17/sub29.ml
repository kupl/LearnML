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
let rec expand : exp -> exp 
= fun exp -> match exp with | LET (x, e1, e2) -> (match e2 with 
							| CONST(n) -> LET (x, e1, e2)
							| VAR(a) -> if a = x then e1 else LET (x, e1, e2)
						        | READ -> LET (x, e1, e2)
						        | _ -> let rec funa x e1 e2 = (match e2 with | CONST(n) -> CONST(n)
												     | VAR(a) -> if a = x then e1 else e2
												     | ADD(e3, e4) -> ADD((funa x e1 e3), (funa x e1 e4))
												     | SUB(e3, e4) -> SUB((funa x e1 e3), (funa x e1 e4))
												     | MUL(e3, e4) -> MUL((funa x e1 e3), (funa x e1 e4))
												     | DIV(e3, e4) -> DIV((funa x e1 e3), (funa x e1 e4))
												     | ISZERO(e3) -> ISZERO((funa x e1 e3))
												     | READ -> READ
												     | IF(e3, e4, e5) -> IF((funa x e1 e3), (funa x e1 e4), (funa x e1 e5))
												     | PROC(x1, e3) -> PROC(x1, (funa x e1 e3))
												     | CALL(e3, e4) -> CALL((funa x e1 e3), (funa x e1 e4))
												     | LET(x2, e3, e4) -> funa x e1 (funa x2 e3 e4)
												     | LETREC(f, x3, e3, e4) -> funa x e1 (funa x3 e3 e4)) in funa x e1 e2) 
		            
                            | LETREC (f, x, e1, e2) -> expand (LET(x, e1, e2))											
			    | _ -> exp



(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
and env = var list

(* environment *)
let empty_env = []
let extend_env x e = x::e
let rec apply_env e x = 
  match e with
  | [] -> false
  | hd::tl -> if x = hd then true else apply_env tl x

let rec check : lambda -> bool
= fun lam -> let rec func lam env = (match lam with | V(a) -> if (apply_env env a) = true then true else false
					   	    | P(a, l) -> (func l (extend_env a env))
					 	    | C(l1, l2) -> ((func l1 env) && (func l2 env))) in func lam empty_env 

















