(*problem 5*)
type e=X
|INT of int
|ADD of e*e
|SUB of e*e
|MUL of e*e
|DIV of e*e
|SIGMA of e*e*e;;

let rec f:e->int->e
=fun e n->match e with
X->INT(n)
|ADD(x,y)->ADD((f x n),(f y n))
|SUB(x,y)->SUB((f x n),(f y n))
|MUL(x,y)->MUL((f x n),(f y n))
|DIV(x,y)->DIV((f x n),(f y n))
|INT(x)->INT(x)
|SIGMA(x,y,z)->SIGMA((f x n),(f y n),( z ));;

let rec calculator :e->int
=fun e->match e with
INT(x)->x
|ADD(x,y)->calculator(x)+calculator(y)
|SUB(x,y)->calculator(x)-calculator(y)
|MUL(x,y)->calculator(x)*calculator(y)
|DIV(x,y)->calculator(x)/calculator(y)
|SIGMA(x,y,z)->
 (if((calculator x)<=(calculator y))
     then calculator(f z (calculator(x)))+calculator(SIGMA((ADD(INT(1),x)),y,z))
     else 0)
|X->raise(Failure"error");;