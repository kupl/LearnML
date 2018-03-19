type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
=fun e ->
match e with  
X->raise (Failure "error")
|INT n->n
|ADD (e1,e2) -> calculator e1+calculator e2
|SUB (e1,e2)-> calculator e1-calculator e2
|MUL (e1,e2)->calculator e1*calculator e2
|DIV (e1,e2)->calculator e1/calculator e2
|SIGMA (e1,e2,e3) ->
( let a= calculator e1 in
  let b= calculator e2 in
  if a>b then 0 else 
    (match e3 with
      X->a
      |INT n->n
      |ADD (e4,e5) -> calculator (SIGMA(e1,e1,e4))+calculator (SIGMA(e1,e1,e5))
      |SUB (e4,e5)-> calculator (SIGMA(e1,e1,e4))-calculator (SIGMA(e1,e1,e5))
      |MUL (e4,e5)-> calculator (SIGMA(e1,e1,e4))*calculator (SIGMA(e1,e1,e5))
      |DIV (e4,e5)-> calculator (SIGMA(e1,e1,e4))/calculator (SIGMA(e1,e1,e5))
      |SIGMA (_,_,_) -> raise (Failure "error") (*시그마의 e3자리에 또 시그마가 있는건 에러*)
    )+calculator (SIGMA(INT (a+1),INT b,e3) )
)

