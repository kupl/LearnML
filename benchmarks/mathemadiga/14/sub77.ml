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


let rec calc (ex,x,xE) =
    match ex with
    | X -> if xE then x else raise FreeVariable
    | INT i -> float_of_int i
    | REAL r -> r
    | ADD (e1,e2) -> (calc(e1,x,xE)) +. (calc(e2,x,xE))
    | SUB (e1,e2) -> (calc(e1,x,xE)) -. (calc(e2,x,xE))
    | MUL (e1,e2) -> (calc(e1,x,xE)) *. (calc(e2,x,xE))
    | DIV (e1,e2) -> (calc(e1,x,xE)) /. (calc(e2,x,xE))
    | SIGMA (i,f,e) ->
            let a = int_of_float (calc(i,x,xE)) in
            let b = int_of_float (calc(f,x,xE)) in
            if (a>b) then 0.
            else (calc(e,float_of_int a,true)) +. (calc ((SIGMA (INT (a+1),INT b, e)), x, xE))
    | INTEGRAL (i,f,e) ->
            let a = (calc(i,x,xE)) in
            let b = (calc(f,x,xE)) in
            if (a>b) then  -1.0 *. (calc ((INTEGRAL (REAL b, REAL a, e)), x, xE))
            else if(b-.a<0.1) then 0.
            else (calc(e,a,true)) +. (calc ((INTEGRAL (REAL (a+.0.1), REAL b, e)), x, xE))

let galculator ex =
    calc(ex,0.,false)
