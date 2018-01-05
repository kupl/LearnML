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

let rec parse (e, v, isFunc) = 
  match e with
    | X -> if isFunc then v
              else raise FreeVariable
    | INT n -> float_of_int n
    | REAL r -> r
    | ADD (x, y) -> parse(x, v, isFunc) +. parse(y, v, isFunc)
    | SUB (x, y) -> parse(x, v, isFunc) -. parse(y, v, isFunc)
    | MUL (x, y) -> parse(x, v, isFunc) *. parse(y, v, isFunc)
    | DIV (x, y) -> parse(x, v, isFunc) /. parse(y, v, isFunc)
    | SIGMA (a, b, f) ->
        let rec mySigma(low, high, func) = 
          if (low = high) then parse(func, float_of_int low, true)
          else if (low > high) then 0.0
          else (mySigma(low + 1, high, func) +. parse(func, float_of_int low, true)) in
        mySigma(int_of_float (parse(a, v, isFunc)), int_of_float (parse(b, v, isFunc)), f)
    | INTEGRAL (a, b, f) ->
        let rec myIntegral(low, high, func) = 
          if (high = low) then 0.0
          else if (high < low) then (-. myIntegral(high, low, func))
          else if (high -. low < 0.1) then 0.0
          else (myIntegral(low +. 0.1, high, func) +. parse(func, low, true) *. 0.1) in
        myIntegral(parse(a, v, isFunc), parse(b, v, isFunc), f)

let galculator e = parse (e, 0.0, false)
