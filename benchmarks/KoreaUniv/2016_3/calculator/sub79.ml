
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec sighelp : int*exp->int = fun(key,exp)->
match exp with
|X->key
|INT(a)->a
|ADD(a,b)->sighelp(key,a)+sighelp(key,b)
|SUB(a,b)->sighelp(key,a)-sighelp(key,b)
|MUL(a,b)->sighelp(key,a)*sighelp(key,b)
|DIV(a,b)->sighelp(key,a)/sighelp(key,b)
|SIGMA(a,b,c)->if calculator(a)>calculator(b) then 0
                else sighelp(calculator(a),c)+calculator(SIGMA(ADD(a,INT(1)),b,c))
and  calculator : exp -> int
  = fun exp ->
match exp with
|X-> raise NotImplemented
|INT(a)->a
|ADD(a,b)->calculator(a)+calculator(b)
|SUB(a,b)->calculator(a)-calculator(b)
|MUL(a,b)->calculator(a)*calculator(b)
|DIV(a,b)->calculator(a)/calculator(b)
|SIGMA(a,b,c)->if calculator(a)>calculator(b) then 0
                else sighelp(calculator(a),c)+calculator(SIGMA(ADD(a,INT(1)),b,c))