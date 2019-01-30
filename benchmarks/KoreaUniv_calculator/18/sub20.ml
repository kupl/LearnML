type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> match exp with
  |INT n->n
  |ADD (x,y)->
    let v1=calculator x in 
    let v2=calculator y in
      (match v1,v2 with 
      |n1, n2 -> n1 + n2)
  |SUB (x,y)->
    let v1=calculator x in 
    let v2=calculator y in
      (match v1,v2 with 
      |n1, n2 -> n1 - n2)
  |MUL (x,y)->
    let v1=calculator x in 
    let v2=calculator y in
      (match v1,v2 with 
      |n1, n2 -> n1 * n2)
  |DIV (x,y)->
    let v1=calculator x in 
    let v2=calculator y in
      (match v1,v2 with 
      |n1, n2 -> n1 / n2);;
  