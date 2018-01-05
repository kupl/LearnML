(*2-5 컴공 2014-10618 이세영*)
type exp = X
          |INT of int
          |REAL of float
          |ADD of exp * exp
          |SUB of exp * exp
          |MUL of exp * exp
          |DIV of exp * exp
          |SIGMA of exp*exp*exp
          |INTEGRAL of exp*exp*exp;;
exception FreeVariable;;
let rec calculator ex fl=
    match ex with
    |X->fl
    |REAL x->x
    |INT x->float_of_int x
    |ADD (x,y)->(calculator x fl)+.(calculator y fl)
    |SUB (x,y)->(calculator x fl)-.(calculator y fl)
    |MUL (x,y)->(calculator x fl)*.(calculator y fl)
    |DIV (x,y)->(calculator x fl)/.(calculator y fl)
    |SIGMA (x,y,z)-> let rec addi (a,b,f)= if a<=b then (calculator f (float_of_int a))+.addi(a+1,b,f) else 0.0
    in addi (int_of_float (calculator x fl),int_of_float (calculator y fl),z)
    |INTEGRAL (x,y,z)->let rec addf (a,b,f)= if a+.0.1<=b then 0.1*.(calculator f a)+.addf(a+.0.1,b,f) else if a<=b then 0.0 else ~-.(addf(b,a,f))
    in addf(calculator x fl,calculator y fl,z);;
let rec galculator ex =
    match ex with
    |X-> raise FreeVariable
    |REAL x->x
    |INT x->float_of_int x
    |ADD (x,y)->(galculator x)+.(galculator y)
    |SUB (x,y)->(galculator x)-.(galculator y)
    |MUL (x,y)->(galculator x)*.(galculator y)
    |DIV (x,y)->(galculator x)/.(galculator y)
    |SIGMA (x,y,z)-> let rec addi (a,b,f)= if a<=b then (calculator f (float_of_int a))+.addi(a+1,b,f) else 0.0
    in addi (int_of_float (galculator x),int_of_float (galculator y),z)
    |INTEGRAL (x,y,z)->let rec addf (a,b,f)= if a+.0.1<=b then 0.1*.(calculator f a)+.addf(a+.0.1,b,f) else if a<=b then 0.0 else ~-.(addf(b,a,f))
    in addf(galculator x, galculator y,z);;
       
