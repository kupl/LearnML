(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec sigcal:int->exp->int
 =fun n e->
match e with
|X->n
|INT x->x
|ADD(a,b)->(sigcal n a)+(sigcal n b)
|SUB(a,b)->(sigcal n a)-(sigcal n b)
|MUL(a,b)->(sigcal n a)*(sigcal n b)
|DIV(a,b)->(sigcal n a)/(sigcal n b)
|SIGMA(a,b,c)->if sigcal n a=sigcal n b then sigcal (sigcal n a) c
else (sigcal (sigcal n a) c)+(sigcal ((sigcal n a)+1) c);;
      
let rec sigma:int->int->exp->int
 =fun n1 n2 e->
if n1=n2 then sigcal n1 e 
else (sigcal n1 e)+(sigma (n1+1) n2 e);;
           
let rec calculator:exp->int
 =fun e->
 match e with
|X->0
|INT x->x
|ADD(a,b)->calculator a+calculator b
|SUB(a,b)->calculator a-calculator b
|MUL(a,b)->calculator a*calculator b
|DIV(a,b)->calculator a/calculator b
|SIGMA(a,b,c)->sigma (calculator a) (calculator b) c;;
