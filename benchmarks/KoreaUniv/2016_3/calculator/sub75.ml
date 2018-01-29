
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec cal : exp * int -> int
  = fun (exp,num) ->
    match exp with
    | X -> num
    | INT(a) -> a
    | ADD (a,b) -> cal (a,num) + cal (b,num)
    | SUB (a,b) -> cal (a,num) - cal (b,num)
    | MUL (a,b) -> cal (a,num) * cal (b,num)
    | DIV (a,b) -> cal (a,num) / cal (b,num)
    | SIGMA(a,b,c) ->
      if cal (a,num) > cal (b,num) then 0
      else
        cal (c, cal(a,num))+ cal (( SIGMA (INT(cal (a,num) + 1),b,c) ) , num)

  let rec calculator : exp -> int
  = fun exp -> (* TODO *)
    cal (exp,0)