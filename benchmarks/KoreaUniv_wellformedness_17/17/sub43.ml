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

type env1 = (var * exp) list

(* test cases *)
let pgm1 = LET ("x", CONST 1, VAR "x")
let pgm2 = 
  LET ("f", PROC ("x", VAR "x"), 
    IF (CALL (VAR "f", ISZERO (CONST 0)), 
        CALL (VAR "f", CONST 11), 
        CALL (VAR "f", CONST 22)))
let pgm3 = LET ("x", ADD (CONST 1, ISZERO (CONST 0)), CONST 2)


let rec replaceVar : var -> env1 -> exp
= fun x env1 ->
  match env1 with
  | (hdvar,hdexp)::tl -> if hdvar=x then hdexp else replaceVar x tl
  | []-> (VAR x)

let rec varExTest : var -> exp -> bool
= fun x e ->
  match e with
  | CONST n -> false
  | VAR y -> y=x
  | ADD (e1,e2) -> (varExTest x e1)||(varExTest x e2)
  | SUB (e1,e2) -> (varExTest x e1)||(varExTest x e2)
  | MUL (e1,e2) -> (varExTest x e1)||(varExTest x e2)
  | DIV (e1,e2) -> (varExTest x e1)||(varExTest x e2)
  | ISZERO e1 -> varExTest x e1
  | READ -> false
  | IF (e1,e2,e3) -> (varExTest x e1)||(varExTest x e2)||(varExTest x e3)
  | LET (y,e1,e2) -> (y=x)||(varExTest x e1)||(varExTest x e2)
  | LETREC (y,z,e1,e2) -> (y=x)||(z=x)||(varExTest x e1)||(varExTest x e2)
  | PROC (y,e1) -> (y=x)||(varExTest x e1)
  | CALL (e1,e2) -> (varExTest x e1)||(varExTest x e2)


let rec doLet : exp -> env1 -> exp
= fun exp env1 ->
  match exp with
  | CONST n -> exp
  | VAR x -> replaceVar x env1
  | ADD (e1,e2) -> ADD (doLet e1 env1, doLet e2 env1)
  | SUB (e1,e2) -> SUB (doLet e1 env1, doLet e2 env1)
  | MUL (e1,e2) -> MUL (doLet e1 env1, doLet e2 env1)
  | DIV (e1,e2) -> DIV (doLet e1 env1, doLet e2 env1)
  | ISZERO e -> ISZERO (doLet e env1) 
  | READ -> exp
  | IF (e1,e2,e3) -> 
      IF (doLet e1 env1, doLet e2 env1, doLet e3 env1)
  | LET (x,e1,e2) -> 
      if varExTest x e2
      then (let newenv=((x,doLet e1 env1)::env1) in (doLet e2 newenv))
      else LET (x,e1,doLet e2 env1)
  | LETREC (f,x,e1,e2) ->
      LETREC(f,x,doLet e1 env1, doLet e2 env1)
      (*if varExTest f e2
            then (let )
          else LETREC (f,x,e1,doLet e2, env1)*)
  | PROC (x,e) -> PROC(x,doLet e env1)
  | CALL (e1,e2) -> CALL (doLet e1 env1, doLet e2 env1)

(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp -> (* TODO *)
  doLet exp []
    


(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type env2 = var list

let rec findVar : var -> env2 -> bool
= fun x env2 ->
  match env2 with
  | hd::tl -> if hd=x then true else findVar x tl
  | [] -> false
  

let rec test : lambda -> env2 -> bool
= fun lam env2 ->
  match lam with
  | V x -> findVar x env2
  | P (x,l) -> let newEnv2=(x::env2) in test l newEnv2
  | C (l1,l2) -> (test l1 env2)&&(test l2 env2)

let rec check : lambda -> bool
= fun lam -> (* TODO *)
  test lam []