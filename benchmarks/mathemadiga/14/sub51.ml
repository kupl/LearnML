exception FreeVariable

type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp



type freevar = FREEVAR
|XX of float


let rec cal expr x  = match expr with
|X ->( match x with 
  |XX f -> f 
  |FREEVAR -> raise FreeVariable)
|INT i -> float_of_int i
|REAL f -> f
|ADD(a, b) -> (cal a x) +. (cal b x)
|SUB(a, b) -> (cal a x) -. (cal b x)
|MUL(a, b) -> (cal a x) *. (cal b x)
|DIV(a, b) -> (cal a x) /. (cal b x)
|SIGMA(i0, n0, f) ->
    let i = float_of_int(int_of_float(cal i0 x)) in 
    let n = float_of_int(int_of_float(cal n0 x)) in
    if(i > n) then 0.0
    else if (i = n) then (cal f (XX i))
    else (cal f (XX i)) +. (cal (SIGMA((REAL (i+.1.0)), (REAL n), f)) x)
|INTEGRAL(i0, n0, f) ->
    let i = (cal i0 x) in
    let n = (cal n0 x) in
    if(i > n) then -.(cal (INTEGRAL((REAL n), (REAL i), f)) x)
    else if(n-.i < 0.1) then 0.0
    else (cal f (XX i))*.0.1 +. (cal (INTEGRAL((REAL (i+.0.1)),(REAL  n), f)) x)

    
let galculator expr = (cal expr FREEVAR)



