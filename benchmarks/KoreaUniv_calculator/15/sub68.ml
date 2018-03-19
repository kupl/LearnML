type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
=fun e -> 0
 
let rec three e1 a= match e1 with 

|X -> a
|INT i -> i
|ADD(v1,v2) ->  (three v1 a) + (three v2 a) 
|SUB(v1,v2) ->  (three v1 a) - (three v2 a) 
|MUL(v1,v2) ->  (three v1 a) * (three v2 a) 
|DIV(v1,v2) ->  (three v1 a) / (three v2 a) 
|SIGMA(v1,v2,v3) -> calculator e1

let rec calculator e= match e with 

|X -> raise (Failure " WRONG")
|INT i -> i
|ADD(e1,e2) ->  (calculator e1) + (calculator e2)
|SUB(e1,e2) ->  (calculator e1) - (calculator e2)
|MUL(e1,e2) ->  (calculator e1) * (calculator e2)
|DIV(e1,e2) ->  (calculator e1) / (calculator e2)
|SIGMA(e1,e2,e3) ->  if ( (calculator e1)< (calculator e2) ) 
then three e3 (calculator e1) + calculator (SIGMA (ADD (e1,INT (1)),e2,e3)) 
else three e3 (calculator e1) 