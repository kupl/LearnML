 type exp =
    X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp 

exception FreeVariable

let rec sigma s e f sum =
  if (s <= e) then
    (sigma ((1. +. float_of_int (int_of_float s) )) (float_of_int (int_of_float e)) f ((f s) +. sum )) 
  else
    (0.0 +. sum)

let rec integral f a b ans isBool =
  let dx = 0.1 in
    if (isBool && (a > b))
      then (integral f b a ans false) (* false *)
    else if (a > b) 
      then (ans +. 0.0)
    else if (a < b)
      then (-1.) *. (integral f (dx +. b ) a (ans +. (dx *. (f b))) false) (* false *)
    else
      (integral f (dx +. a) b (ans +. (dx *. (f a))) false) (* false *)


let rec myGalculator exp z =
    match exp with
      X                        -> (z ())
    | INT i                    -> (float_of_int i)
    | REAL f                   -> (f)
    | ADD (exp1, exp2)         -> ((myGalculator exp1 z) +. (myGalculator exp2 z))
    | SUB (exp1, exp2)         -> ((myGalculator exp1 z) -. (myGalculator exp2 z))
    | MUL (exp1, exp2)         -> ((myGalculator exp1 z) *. (myGalculator exp2 z))
    | DIV (exp1, exp2)         -> ((myGalculator exp1 z) /. (myGalculator exp2 z))
    | SIGMA (exp1, exp2, exp3) -> (sigma (myGalculator exp1 z)
                                    (myGalculator exp2 z)
                                    (fun a -> (myGalculator exp3 (fun () -> a)))
                                    (0.))
    | INTEGRAL (exp1, exp2, exp3) -> (integral
                                       (fun a -> (myGalculator exp3 (fun () -> a)))
                                       (myGalculator exp1 z)
                                       (myGalculator exp2 z)
                                       (0.)
                                       (true))
let default = fun () -> raise FreeVariable
let rec galculator exp = (myGalculator exp default)

