type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreeVariable

let rec vsigma : (int * int * (float -> float)) -> float = fun (a, b, f) ->
 if (a > b) then 0.0
 else (f (float_of_int a)) +. vsigma(a + 1, b, f)
 
let rec vintegral : (float * float * (float -> float)) -> float = fun (a, b, f) ->
 if (a > b) then -. vintegral (b, a, f)
 else if ((b -. 0.1) < a) then 0.0
 else ((f a) *. 0.1) +. vintegral(a +. 0.1, b, f)

let rec expval : exp -> ((float -> float) * bool) = fun x ->
 match x with
 | X -> ((fun y -> y), true)
 | INT n -> ((fun y -> float_of_int n), false)
 | REAL n -> ((fun y -> n), false)
 | ADD (x1, x2) -> ((fun y -> (((fst (expval x1)) y) +. ((fst (expval x2)) y))), (snd (expval x1)) || (snd (expval x2)))
 | SUB (x1, x2) -> ((fun y -> (((fst (expval x1)) y) -. ((fst (expval x2)) y))), (snd (expval x1)) || (snd (expval x2)))
 | MUL (x1, x2) -> ((fun y -> (((fst (expval x1)) y) *. ((fst (expval x2)) y))), (snd (expval x1)) || (snd (expval x2)))
 | DIV (x1, x2) -> ((fun y -> (((fst (expval x1)) y) /. ((fst (expval x2)) y))), (snd (expval x1)) || (snd (expval x2)))
 | SIGMA (x1, x2, x3) -> ((fun y -> vsigma (int_of_float ((fst (expval x1)) y), int_of_float ((fst (expval x2)) y), fst (expval x3))), (snd (expval x1)) || (snd (expval x2)))
 | INTEGRAL (x1, x2, x3) -> ((fun y -> vintegral ((fst (expval x1)) y, (fst (expval x2)) y, fst (expval x3))), (snd (expval x1)) || (snd (expval x2)))
 
let galculator : exp -> float = fun x ->
 if snd (expval x) then raise FreeVariable
 else (fst (expval x)) 0.0
