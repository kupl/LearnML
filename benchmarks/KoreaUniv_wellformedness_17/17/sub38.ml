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
(*let pgm7 = LET("x", LET("y", LET("x", CONST 200, SUB(VAR "x", CONST 4)), ADD(VAR "y", CONST 3)), MUL(VAR "x", CONST 100))

let pgm4 = LET("x", CONST 200, LET("f", PROC("z", SUB(VAR"z",VAR"x")), LET("k", CONST 100, LET("g", PROC("z", SUB(VAR "z",VAR "k")), SUB(CALL(VAR "f", CONST 1), CALL(VAR "g", CONST 1))))))
let pgm5 = LET("x", CONST 200, LET("f", PROC("z", SUB(VAR"z", VAR"A")), CALL(VAR "f", CONST 1)))

let pgm6 = LET("x", LET("y", CONST 200, ADD(CONST 3, VAR "y")), ADD(VAR "x",CONST 4))
*)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x =
match e with
|[] -> x
|(y,v)::tl-> if x = y then v else apply_env tl x

let rec find env exp =
match env with
|[] -> false
|(y,v)::tl -> if exp = y then true else find tl exp

let rec occurence exp env =
match exp with
|CONST n -> find env (CONST n)
|VAR x-> find env (VAR x)
|IF (e1, e2, e3) -> (occurence e1 env) || (occurence e2 env) || (occurence e3 env)
|ISZERO e -> occurence e env
|LET(x, e1, e2)-> if (find env (VAR x)) == true then false else (occurence e1 env)||(occurence e2 env)
|LETREC(f, x, e1, e2) -> (occurence e1 env) || (occurence e2 env)
|READ -> false
|PROC(x, e) -> occurence e env
|CALL(e1, e2) -> (occurence e1 env)||(occurence e2 env)
|ADD(e1, e2) -> (occurence e1 env) || (occurence e2 env)
|SUB(e1, e2) -> (occurence e1 env)||(occurence e2 env)
|DIV(e1, e2) -> (occurence e1 env) || (occurence e2 env)
|MUL(e1, e2) -> (occurence e1 env) ||(occurence e2 env)


(* You can define datatypes and helper functions as necessary *)
let rec expand : exp -> exp 
= fun exp ->
let rec expand2 exp env
= (match exp with
|VAR x -> apply_env env (VAR x)
|LET(x, e1, e2)-> let newe1 = expand2 e1 env in
                  let newenv = extend_env ((VAR x), newe1) env in
                  let testtt = expand2 e2 newenv in
                  if (occurence e2 [(VAR x, newe1)]) == false then LET(x, newe1, testtt) else testtt                    
|CONST n -> apply_env env (CONST n)
|ADD(e1, e2) -> let v1 = expand2 e1 env in
                  let v2 = expand2 e2 env in
                    ADD(v1, v2)
|SUB(e1, e2) -> let v1 = expand2 e1 env in
                  let v2 = expand2 e2 env in
                    SUB(v1, v2)
|DIV(e1, e2) -> let v1 = expand2 e1 env in
                  let v2 = expand2 e2 env in
                    DIV(v1, v2)

|MUL(e1, e2) -> let v1 = expand2 e1 env in
                  let v2 = expand2 e2 env in
                      MUL(v1, v2)
|READ -> READ
|ISZERO e -> let v1 = expand2 e env in
                ISZERO v1
|IF(e1, e2, e3) -> let v1 = expand2 e1 env in
                      let v2 = expand2 e2 env in
                        let v3 = expand2 e3 env in
                          IF(v1, v2, v3)
|LETREC(f, x, e1, e2) -> let v1 = expand2 e1 env in
                          let v2 = expand2 e2 env in
                            LETREC(f, x, v1, v2)
|PROC(x, e) -> let v1 = expand2 e env in
                  PROC(x, v1)
|CALL(e1, e2) -> let v1 = expand2 e1 env in
                  let v2 = expand2 e2 env in
                    CALL(v1,v2)
)                
in expand2 exp []

(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string
(*
let quiz1 = P("a", V "a")
let quiz2 = P("a", P("a", V "a"))
let quiz3 = P("a", P("b", C(V "a", V "b")))
let quiz4 = P("a", C(V "a", P("b", V "a")))

let quiz5 = P("a", V "b")
let quiz6 = P("a", C (V "a", P("b", V "c")))
let quiz7 = P("a", P("b", C (V "a", V "c")))
let quiz8 = P("b", P("a", V "a"))

let quiz9 = P("a", C(P("d", C(V "d", P("a", C(V"a",V"d")))), V "a"))
*)
let extenv x env = x :: env
let rec search env lam =
match env with
|[] -> false
|a::tl -> if lam = a then true else search tl lam

let rec checklambda lambda env
= match lambda with
|V x -> search env x
|P(x, l) -> let newenv1 = extenv x env in
            checklambda l newenv1
|C(l1, l2) -> checklambda l1 env && checklambda l2 env

let rec check : lambda -> bool
= fun lam -> checklambda lam []
