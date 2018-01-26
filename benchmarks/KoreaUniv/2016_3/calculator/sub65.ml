
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  (*count는 SIGMA의 X때문에 쓰임 SIGMA빼고는 걍 아무값이나 없어도 된다*)
  let rec cal : exp -> int -> int
  = fun exp count ->

    match exp with
    |X -> count
    |INT(integer) -> integer 
    |ADD(e1, e2) -> (cal e1 count) + (cal e2 count)
    |SUB(e1, e2) -> (cal e1 count) - (cal e2 count)
    |MUL(e1, e2) -> (cal e1 count) * (cal e2 count)
    |DIV(e1, e2) -> (cal e1 count) / (cal e2 count)
    |SIGMA(start, last, expression) -> sigma (cal start 0) (cal last 0) expression count 


  and sigma : int -> int-> exp -> int -> int
  = fun count last expression sum->
    if( (count-1)=last ) then sum(*루프 끝*)
    else sigma (count+1) last  expression (sum + (cal expression count))   


  let calculator : exp -> int
  = fun exp -> (* TODO *)
    cal exp 0