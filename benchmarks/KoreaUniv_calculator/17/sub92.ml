(*problem 5*)
type exp = X
| INT of int
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp

let e = SIGMA(INT 1, INT 5, ADD(X,X));;

let rec calculator : exp -> int = fun e ->
match e with
|X -> calculator X
|INT n -> n
|ADD (e1,e2) -> ((calculator e1) + (calculator e2))
|SUB (e1,e2) -> ((calculator e1) - (calculator e2))
|MUL (e1,e2) -> ((calculator e1) * (calculator e2))
|DIV (e1,e2) -> ((calculator e1) / (calculator e2))
|SIGMA (e1,e2,e3) -> 
  (match e1,e2, e3 with
  |INT n1, INT n2, _ -> 
    if(n1<n2+1) then 
      (match e3 with 
      |X -> n1 + calculator(SIGMA(INT (n1+1), e2, e3))
      |INT n -> n + calculator(SIGMA(INT (n1+1), e2, e3))
      |ADD (e4,e5) -> calculator(SIGMA(e1, e1, e4)) + calculator(SIGMA(e1, e1, e5)) + calculator(SIGMA(INT (n1+1), e2, e3))
      |SUB (e4,e5) -> calculator(SIGMA(e1, e1, e4)) - calculator(SIGMA(e1, e1, e5)) + calculator(SIGMA(INT (n1+1), e2, e3))
      |MUL (e4,e5) -> calculator(SIGMA(e1, e1, e4)) * calculator(SIGMA(e1, e1, e5)) + calculator(SIGMA(INT (n1+1), e2, e3))
      |DIV (e4,e5) -> calculator(SIGMA(e1, e1, e4)) / calculator(SIGMA(e1, e1, e5)) + calculator(SIGMA(INT (n1+1), e2, e3))
      |SIGMA (e4,e5,e6) -> raise (Failure ("type error")))
   else 0
  |_ -> raise (Failure ("type error")))