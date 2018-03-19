
type exp = 
      | X
      | INT of int
      | ADD of exp * exp
      | SUB of exp * exp
      | MUL of exp * exp
      | DIV of exp * exp
      | SIGMA of exp * exp * exp

let rec calculator f = match f with
| X -> raise (Failure"X must be int type.")
| INT x -> x
| ADD (x,y) -> let v1 = calculator x in let v2 = calculator y in v1+v2
| SUB (x,y) -> let v1 = calculator x in let v2 = calculator y in v1-v2
| MUL (x,y) -> let v1 = calculator x in let v2 = calculator y in v1*v2
| DIV (x,y) -> let v1 = calculator x in let v2 = calculator y in v1/v2
| SIGMA (x,y,z) -> let v1 = calculator x in let v2 = calculator y in
if v1=v2 then sigma z v1 else (sigma z v1)+(calculator (SIGMA (ADD (x, INT 1),y,z)))
and sigma fu x1 = match fu with
| X -> x1
| INT q -> q
| ADD (q,w) -> let b1 = sigma q x1 in let b2 = sigma w x1 in b1+b2
| SUB (q,w) -> let b1 = sigma q x1 in let b2 = sigma w x1 in b1-b2
| MUL (q,w) -> let b1 = sigma q x1 in let b2 = sigma w x1 in b1*b2
| DIV (q,w) -> let b1 = sigma q x1 in let b2 = sigma w x1 in b1/b2
| SIGMA (q,w,r) -> let b1 = sigma q x1 in let b2 = sigma w x1 in
if b1=b2 then sigma r b1 else (sigma r b1)+(calculator (SIGMA (ADD(q,INT 1),w,r)));;

