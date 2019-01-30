type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec inverter : exp -> int -> int
= fun exp1 value -> match exp1 with
  |X -> value
  |INT n -> n
  |ADD (e1, e2) -> inverter e1 value + inverter e2 value
  |SUB (e1, e2) -> inverter e1 value - inverter e2 value
  |MUL (e1, e2) -> inverter e1 value * inverter e2 value
  |DIV (e1, e2) -> inverter e1 value / inverter e2 value
  |SIGMA (e1, e2, e3) -> if( (inverter e1 value) <= (inverter e2 value) ) then (inverter e3 value) + (inverter (SIGMA (ADD (e1, INT 1), e2, e3)) (value + 1)) else 0;;
  
  let rec calculator : exp -> int
= fun exp -> match exp with
  X -> 1
  |INT n -> n
  |ADD (e1, e2) -> calculator (e1) + calculator(e2)
  |SUB (e1, e2) -> calculator (e1) - calculator(e2)
  |MUL (e1, e2) -> calculator (e1) * calculator(e2)
  |DIV (e1, e2) -> calculator (e1) / calculator(e2)
  |SIGMA (e1, e2 , e3) -> if( calculator (e1) <= calculator (e2) ) then inverter (SIGMA(e1, e2, e3)) (calculator e1) else 0;;
