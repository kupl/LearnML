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
= fun exp -> (* TODO *)
  let rec help exp env =
  (*  match exp with
    | V a -> List.mem a env
    | P (a, b) -> help b (a::env)
    | C (a, b) -> (help a env && help b env)
      in help lam []
 *)
    match exp with
    | CONST n -> CONST n
    | VAR v -> VAR v
    | ADD (e1,e2) -> 
      ADD (e1,e2)
    | SUB (e1,e2) ->
      SUB (e1,e2)
    | MUL (e1,e2) ->
      MUL (e1,e2)
    | DIV (e1,e2) ->
      DIV (e1,e2)
    | ISZERO e ->
      ISZERO e
    | READ -> 
      READ
    | IF (e1,e2,e3) ->
      IF (e1,e2,e3)
    | LET (x,e1,e2) ->
      (match e2 with
      | CONST n -> exp
      | VAR v -> if (x=v) then e1 else exp
      | _ -> e1)
    | LETREC (f,x,e1,e2) ->
      e1
    | PROC (x,e) ->
      PROC (x,e)
    | CALL (e1,e2) ->
      CALL (e1,e2)
    in help exp []

(* let rec string sen = 
  match sen with
  | VAR v -> v
  | TyBool -> "bool"
  | TyFun (t1,t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TyVar x -> x *)

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check : lambda -> bool
= fun lam ->  (* TODO *)
  let rec help lam env =
    match lam with
    | V a -> List.mem a env
    | P (a, b) -> help b (a::env)
    | C (a, b) -> (help a env && help b env)
      in help lam []
