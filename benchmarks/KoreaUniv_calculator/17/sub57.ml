(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let rec get : exp->int->int = fun exp v->
match exp with|X->v|INT n->n
|ADD (n1,n2)->let a = (get n1 v) in let b = (get n2 v) in (a+b)
|SUB (n1,n2)->let a = (get n1 v) in let b = (get n2 v) in (a-b)
|MUL (n1,n2)->let a = (get n1 v) in let b = (get n2 v) in (a*b)
|DIV (n1,n2)->let a = (get n1 v) in let b = (get n2 v) in (a/b)
|SIGMA (x,y,e)->let a = (get x v) in let b = (get y v) in
	if a>b then 0 else (get e a)+get (SIGMA (INT (a+1),INT b,e)) (a+1);;

let rec calculator : exp -> int
= fun e -> match e with|X-> raise (Failure "Type Error")|INT n->n
|ADD (n1,n2)->let a = calculator n1 in let b = calculator n2 in (a+b)
|SUB (n1,n2)->let a = calculator n1 in let b = calculator n2 in (a-b)
|MUL (n1,n2)->let a = calculator n1 in let b = calculator n2 in (a*b)
|DIV (n1,n2)->let a = calculator n1 in let b = calculator n2 in (a/b)
|SIGMA (n1,n2,s)->let a = calculator n1 in get e a;;
