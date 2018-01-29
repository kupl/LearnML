(*problem5*)

type exp =X
|INT of int
|ADD of exp *exp
|SUB of exp *exp
|MUL of exp *exp
|DIV of exp * exp
|SIGMA of exp*exp*exp;;

let rec calculator : exp -> int = fun e ->
match e with
|X -> calculator X
|INT a->a
|ADD(a,b)-> ((calculator a)+(calculator b))
|MUL(a,b)-> ((calculator a) *  (calculator b))
|SUB(a,b)-> ((calculator a) - (calculator b))
|DIV(a,b)-> ((calculator a) / (calculator b))
|SIGMA(a,b,c)-> (match a,b,c with 
    |INT n1 , INT n2, _ ->
    if(n1<n2+1) then
    (match c with
     |X -> n1 + calculator(SIGMA(INT(n1+1), b,c))
     |INT a -> a + calculator(SIGMA(INT(n1+1),b,c))
     |ADD (d,e) -> calculator(SIGMA(a,a,d))+calculator(SIGMA(a,a,e)) + calculator(SIGMA(INT (n1+1),b,c))
     |SUB (d,e) -> calculator(SIGMA(a,a,d)) - calculator(SIGMA(a,a,e)) + calculator(SIGMA(INT (n1+1),b,c))
     |MUL (d,e) ->calculator(SIGMA(a,a,d)) * calculator(SIGMA(a,a,e)) + calculator(SIGMA(INT(n1+1),b,c))
     |DIV (d,e) -> calculator(SIGMA(a,a,d)) / calculator(SIGMA(a,a,e)) + calculator(SIGMA(INT(n1+1),b,c))
     |SIGMA(d,e,f) -> raise(Failure("sigma is too many")))
    else 0
    |_-> raise(Failure("error")));;
