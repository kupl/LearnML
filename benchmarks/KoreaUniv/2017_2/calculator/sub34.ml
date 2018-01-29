(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

type value5 = Int of int

type env5 = (value5)

let apply_env5 e=
  match e with
  Int n-> Int n

let rec eval5 :exp->env5->value5
= fun exp env ->
  match exp with
  X-> apply_env5 env
  |INT n-> Int n
  |ADD(e1,e2)->
    let v1 = eval5 e1 env in
    let v2 = eval5 e2 env in
    (match v1,v2 with
    Int n1,Int n2 -> Int(n1+n2))
  |SUB(e1,e2)->
    let v1 = eval5 e1 env in
    let v2 = eval5 e2 env in
    (match v1, v2 with
    Int n1, Int n2-> Int(n1-n2))
  |MUL (e1,e2) ->
    let v1 = eval5 e1 env in
    let v2 = eval5 e2 env in
    (match v1, v2 with
    Int n1, Int n2-> Int (n1*n2))
  |DIV (e1,e2)->
    let v1 = eval5 e1 env in
    let v2 = eval5 e2 env in
    (match v1, v2 with
    Int n1, Int n2-> Int (n1/n2))
  |SIGMA(e1,e2,e3) ->
    let v1 = eval5 e1 env in
    let v2 = eval5 e2 env in
    let v3 = eval5 e3 v1 in
    if v1 <= v2 then
      match v1,v2,v3 with
      Int n1, Int n2, Int n3 -> eval5 (ADD((INT n3),SIGMA(INT (n1+1),INT n2,e3))) (Int n1)
    else Int 0





let calculator : exp -> int
= fun e ->
let v0 = eval5 e (Int 0) in
match v0 with
Int n -> n