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
let rec _sigma(a,b,e) = 
        if (a > b) then INT(0)
        else match e with
                X -> ADD(INT(a),_sigma(a+1,b,e))
               |INT i -> ADD(INT(i),_sigma(a+1,b,e))
               |REAL r -> ADD(REAL(r),_sigma(a+1,b,e))
               |ADD(e1,e2) -> ADD(_sigma(a,b,e1),_sigma(a,b,e2))
               |SUB(e1,e2) -> SUB(_sigma(a,b,e1),_sigma(a,b,e2))
               |MUL(e1,e2) -> ADD(MUL(_sigma(a,a,e1),_sigma(a,a,e2)),_sigma(a+1,b,e))
               |DIV(e1,e2) -> ADD(DIV(_sigma(a,a,e1),_sigma(a,a,e2)),_sigma(a+1,b,e))
               |SIGMA(e1,e2,e3) -> ADD(SIGMA(_sigma(a,a,e1),_sigma(a,a,e2),e3),_sigma(a+1,b,e))
               |INTEGRAL(e1,e2,e3) ->
                               ADD(INTEGRAL(_sigma(a,a,e1),_sigma(a,a,e2),e3),_sigma(a+1,b,e))
let rec _integral(a,b,e) = 
        if (a > b) then SUB(INT(0),_integral(b,a,e))
        else if (b -. a < 0.1) then INT(0)
        else match e with
                X -> ADD(MUL(REAL(a),REAL(0.1)),_integral(a+.0.1,b,e))
               |INT i -> ADD(MUL(REAL(float_of_int(i)),REAL(0.1)),_integral(a+.0.1,b,e))
               |REAL r -> ADD(MUL(REAL(r),REAL(0.1)),_integral(a+.0.1,b,e))
               |ADD(e1,e2) -> ADD(_integral(a,b,e1),_integral(a,b,e2))
               |SUB(e1,e2) -> SUB(_integral(a,b,e1),_integral(a,b,e2))
               |MUL(e1,e2) ->
                               ADD(MUL(MUL(_integral(a,a+.0.1,e1),_integral(a,a+.0.1,e2)),REAL(0.1)),_integral(a+.0.1,b,e))
               |DIV(e1,e2) ->
                               ADD(MUL(DIV(_integral(a,a+.0.1,e1),_integral(a,a+.0.1,e2)),REAL(0.1)),_integral(a+.0.1,b,e))
               |SIGMA(e1,e2,e3) ->
                               ADD(MUL(SIGMA(_integral(a,a+.0.1,e1),_integral(a,a+.0.1,e2),e3),REAL(0.1)),_integral(a+.0.1,b,e))
               |INTEGRAL(e1,e2,e3) ->
                               ADD(MUL(INTEGRAL(_integral(a,a+.0.1,e1),_integral(a,a+.0.1,e2),e3),REAL(0.1)),_integral(a+.0.1,b,e))
let rec galculator exp = match exp with
        X -> raise FreeVariable
        |INT i -> float_of_int(i)
        |REAL r -> r
        |ADD(e1,e2) -> galculator(e1) +. galculator(e2)
        |SUB(e1,e2) -> galculator(e1) -. galculator(e2)
        |MUL(e1,e2) -> galculator(e1) *. galculator(e2)
        |DIV(e1,e2) -> galculator(e1) /. galculator(e2)
        |SIGMA(e1,e2,e3) ->
                        galculator(_sigma(int_of_float(galculator(e1)),int_of_float(galculator(e2)),e3))
        |INTEGRAL(e1,e2,e3) ->
                        galculator(_integral(galculator(e1),galculator(e2),e3))
         
