type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp


type value = FREEVAL
           | FLOATVAL of float

exception FreevarError
exception DividedByZero

let rec mathemadiga exp =

    let valuex x =
    match x with
      FREEVAL -> raise FreevarError
    | FLOATVAL(v) -> v
    in

    let rec mathemadigaX exp x =
    match exp with
      X -> valuex x
    | INT(int) -> float_of_int int
    | REAL(float) -> float
    | ADD(e1, e2) -> (mathemadigaX e1 x) +. (mathemadigaX e2 x)
    | SUB(e1, e2) -> (mathemadigaX e1 x) -. (mathemadigaX e2 x)
    | MUL(e1, e2) -> (mathemadigaX e1 x) *. (mathemadigaX e2 x)
    | DIV(e1, e2) -> (mathemadigaX e1 x) /. (mathemadigaX e2 x)
    | SIGMA(INT(i1), INT(i2), e3) -> if i1 = i2
    				then (mathemadigaX e3 (FLOATVAL(float_of_int i1)))
				else (mathemadigaX e3 (FLOATVAL(float_of_int i1))) +. (mathemadigaX (SIGMA(INT(i1 + 1), INT(i2), e3)) x)
    | SIGMA(_,_,_) -> raise FreevarError 
    | INTEGRAL(e1, e2, e3) -> if (mathemadigaX e1 x) > (mathemadigaX e2 x) then (-. (mathemadigaX (INTEGRAL(e2, e1, e3)) x))
                              else if ((mathemadigaX e2 x) -. (mathemadigaX e1 x)) < 0.1
    				   then (mathemadigaX e3 (FLOATVAL(mathemadigaX e1 x))) *. ((mathemadigaX e2 x) -. (mathemadigaX e1 x))
				   else ((0.1 *. (mathemadigaX e3 (FLOATVAL(mathemadigaX e1 x)))) +. (mathemadigaX (INTEGRAL( REAL((mathemadigaX e1 x) +. 0.1), e2, e3)) x))


   				
    in

    match exp with
      X -> raise FreevarError
    | INT(int) -> float_of_int int
    | REAL(float) -> float
    | ADD(e1, e2) -> (mathemadiga e1) +. (mathemadiga e2)
    | SUB(e1, e2) -> (mathemadiga e1) -. (mathemadiga e2)
    | MUL(e1, e2) -> (mathemadiga e1) *. (mathemadiga e2)
    | DIV(e1, e2) -> (mathemadiga e1) /. (mathemadiga e2)
    | SIGMA(e1, e2, e3) -> (mathemadigaX exp FREEVAL)
    | INTEGRAL(e1, e2, e3) -> (mathemadigaX exp FREEVAL)


