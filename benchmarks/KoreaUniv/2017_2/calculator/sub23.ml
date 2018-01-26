
(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec cal_sigma 
= fun e iter eon ->
if iter > eon then 0
else match e with
  X -> iter
| INT k -> k
| ADD (e1,e2) -> (cal_sigma e1 iter eon) + (cal_sigma e2 iter eon)
| SUB (e1,e2) -> (cal_sigma e1 iter eon) - (cal_sigma e2 iter eon)
| MUL (e1,e2) -> (cal_sigma e1 iter eon) * (cal_sigma e2 iter eon)
| DIV (e1,e2) -> (cal_sigma e1 iter eon) / (cal_sigma e2 iter eon)
| SIGMA (est,eed,form) ->
  let st = (cal_sigma est 0 0) in
  let ed = (cal_sigma eed 0 0) in
  if st > ed then 0
  else if st = ed then (cal_sigma form st ed)
  else (cal_sigma form st ed) + (cal_sigma (SIGMA(INT (st+1),INT ed,form)) 0 0)

let calculator : exp -> int
= fun e -> cal_sigma e 0 0
