
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun exp ->  let rec calc : exp * exp -> int 
=fun (v, e)-> match e with 
X -> calc(INT 0, v)
| INT(i) -> i
| ADD (e1,e2) -> calc(v,e1)+calc(v,e2)
| SUB (e1,e2) -> calc(v,e1)-calc(v,e2)
| MUL (e1,e2) -> calc(v,e1)*calc(v,e2)
| DIV (e1,e2) -> calc(v,e1)/calc(v,e2)
| SIGMA (i,j,e3) -> if i=j then calc(i,e3) else calc(v, (ADD(INT (calc(i,e3)), SIGMA( (INT (calc(INT 0,i)+1)),j,e3))))
in calc (INT 0,exp)