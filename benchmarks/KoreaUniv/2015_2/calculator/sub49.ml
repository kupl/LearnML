type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp


let rec exptoint : exp*int-> int
=fun (e,a) ->
( match e with
X -> a
| INT x -> x
| ADD (x,y) -> exptoint (x,a)+ exptoint (y,a)
| SUB (x,y) -> exptoint (x,a)- exptoint (y,a)
| MUL (x,y) -> exptoint (x,a)* exptoint (y,a)
| DIV (x,y) -> exptoint (x,a)/ exptoint (y,a)
| SIGMA(x,y,f) -> 
(
  if(exptoint (x,a)<exptoint (y,a)) then exptoint (f,exptoint (x,a)) + exptoint (SIGMA(INT ((exptoint (x,a))+1),y,f),(exptoint (x,a))+1)
  else if (exptoint (x,a)=exptoint (y,a)) then exptoint (f,exptoint(x,a))
  else 0 )
)

let calculator : exp -> int
=fun e -> exptoint (e,0)