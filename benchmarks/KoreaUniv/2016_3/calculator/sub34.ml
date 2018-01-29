(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
    = fun exp ->
  match exp with
  | X -> raise NotImplemented
  | INT num -> num
  | ADD(m, n) -> (calculator m) + (calculator n)
  | SUB(m, n) -> (calculator m) - (calculator n)
  | MUL(m, n) -> (calculator m) * (calculator n)
  | DIV(m, n) -> (calculator m) / (calculator n)
  | SIGMA(m, n, p) ->
  if (calculator m) > (calculator n) then 0
  else if (calculator m) = (calculator n) then
  begin
   let rec cal : exp*exp -> int
    = fun(ex, i) ->
    match ex with
    | X -> cal(i, i)
    | INT num -> num
    | ADD(a, b) -> cal(a, i) + cal(b, i)
    | SUB(a, b) -> cal(a, i) - cal(b, i)
    | MUL(a, b) -> cal(a, i) * cal(b, i)
    | DIV(a, b) -> cal(a, i) / cal(b, i)
    | SIGMA(a, b, c) -> calculator c
  in
  cal(p, m)
  end
  else calculator(SIGMA(ADD(m, INT 1), n, p)) + calculator(SIGMA(m, m, p))