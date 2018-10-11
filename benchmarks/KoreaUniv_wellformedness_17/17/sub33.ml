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

type env = (var * exp) list

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)

(* You can define datatypes and helper functions as necessary *)
let rec extend_env x a e = (x, a) :: e

let rec exist env v = match env with
                      | [] -> false
                      | (a, b) :: tl -> if (a = v) then true else exist tl v

let rec find x env = match env with
                     | [] -> raise (Failure "Env error")
                     | (v, a) :: tl -> if(x = v) then a else find x tl

let rec checking x exp = match exp with
                      | CONST n -> false
                      | VAR a -> if a = x then true else false
                      | ADD (e1, e2) -> checking x e1 || checking x e2
                      | SUB (e1, e2) -> checking x e1 || checking x e2
                      | MUL (e1, e2) -> checking x e1 || checking x e2
                      | DIV (e1, e2) -> checking x e1 || checking x e2
                      | ISZERO e -> checking x e
                      | READ -> false
                      | IF (e1, e2, e3) -> checking x e1 || checking x e2 || checking x e3
                      | LET (a, e1, e2) -> checking x e1 || checking x e2
                      | LETREC (f, a, e1, e2) -> checking x e1 || checking x e2
                      | PROC (a, e) -> checking x e
                      | CALL (e1, e2) -> checking x e1 || checking x e2

let rec expand : exp -> exp 
= fun exp ->
  let rec sub e en =
  match e with
  | CONST n -> CONST n
  | VAR x -> if (exist en x = false) then VAR x else find x en
  | ADD (e1, e2) -> ADD (sub e1 en, sub e2 en)
  | SUB (e1, e2) -> SUB (sub e1 en, sub e2 en)
  | MUL (e1, e2) -> MUL (sub e1 en, sub e2 en)
  | DIV (e1, e2) -> DIV (sub e1 en, sub e2 en)
  | ISZERO e -> ISZERO (sub e en)
  | READ -> READ
  | IF (e1, e2, e3) -> IF (sub e1 en, sub e2 en, sub e3 en)
  | LET (x, e1, e2) -> let en' = extend_env x e1 en in
                       if checking x e2 = true then sub e2 en'
                     else e
  | LETREC (f, x, e1, e2) -> LETREC (f, x, sub e1 en, sub e2 en)
  | PROC (x, e) -> PROC (x, sub e en)
  | CALL (e1, e2) -> CALL (sub e1 en, sub e2 en)

in sub exp []


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam -> 
  let extend v env = v :: env in
  let find v env = List.mem v env in
  let rec check_env lambda env =
    match lambda with
    | V v -> find v env
    | P (v, l) -> let newenv = extend v env in
                  check_env l newenv
    | C (l1, l2) -> check_env l1 env && check_env l2 env

  in check_env lam []