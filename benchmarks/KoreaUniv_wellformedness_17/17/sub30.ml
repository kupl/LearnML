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
type env2 = var list

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
   match e with
    | [] -> raise (Failure (x ^ " is unbound in Env"))
    | (y,v)::tl -> if x = y then v else apply_env tl x

(* environment2 *)
let empty_env2 = []
let extend_env2 x e = x::e
let rec apply_env2 e x =
   match e with
    | [] -> false
    | y::tl -> if x = y then true else apply_env2 tl x
        
let rec expand : exp -> exp 
= fun exp -> let rec help_expand exp env
             = match exp with
                | CONST(x) -> CONST(x)
                | VAR(x) -> (apply_env env x)
                | ADD(x,y) -> ADD((help_expand x env), (help_expand y env))
                | SUB(x,y) -> SUB((help_expand x env), (help_expand y env))
                | MUL(x,y) -> MUL((help_expand x env), (help_expand y env))
                | DIV(x,y) -> DIV((help_expand x env), (help_expand y env))
                | READ -> let x = read_int() in CONST(x)
                | ISZERO(x) -> ISZERO(help_expand x env)
                | IF(x,y,z) -> IF((help_expand x env), (help_expand y env), (help_expand z env))
                | LET(x,y,z) -> (let rec check_x exp2
                                 = match exp2 with
                                    | CONST(a) -> false
                                    | VAR(a) -> if a = x then true
                                                else false
                                    | ADD(a,b) -> (check_x a)||(check_x b)
                                    | SUB(a,b) -> (check_x a)||(check_x b)
                                    | MUL(a,b) -> (check_x a)||(check_x b)
                                    | DIV(a,b) -> (check_x a)||(check_x b)
                                    | READ -> false
                                    | ISZERO(a) -> (check_x a)
                                    | IF(a,b,c) -> (check_x a)||(check_x b)||(check_x c)
                                    | LET(a,b,c) -> (check_x b)||(check_x c)
                                    | LETREC(a,b,c,d) -> (check_x c)||(check_x d)
                                    | PROC(a,b) -> (check_x b)
                                    | CALL(a,b) -> (check_x a)||(check_x b)
                                  in (if (check_x z) then (help_expand z (extend_env (x,y) env))
                                     else LET(x,y,z)))
                | LETREC(w,x,y,z) -> LETREC(w,x,y,z)
                | PROC(x,y) -> PROC(x,y)
                | CALL(x,y) -> CALL((help_expand x env), (help_expand y env))
              in (help_expand exp empty_env)

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> let rec help_check lam env 
              = match lam with
                 | V(x) -> if (apply_env2 env x) == true then true
                           else false
                 | P(x,y) -> (help_check y (extend_env2 x env))
                 | C(x,y) -> (help_check x env)&&(help_check y env)
              in (help_check lam empty_env2)
