type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

 module SMap=Map.Make

let rec aux_sigma var b env e =
  let rec aux env acc =
    let v = SMap var b env e in
    if v > b then acc
    else 
      let res = calculator env e in
      aux (SMap.add var (v + 1) env) (acc + res)
  in aux env 0

and calculator env=function
  | INT x -> x
  | ADD (e1, e2) -> calculator env e1 + calculator env e2
  | SUB (e1, e2) -> calculator env e1 - calculator env e2
  | MUL (e1, e2) -> calculator env e1 * calculator env e2
  | DIV (e1, e2) -> calculator env e1 / calculator env e2
  | SIGMA(a,b,c)->if calculator env e1<=calculator env e2 then  calculator sigma else calculator env e1+1;;
  

    


