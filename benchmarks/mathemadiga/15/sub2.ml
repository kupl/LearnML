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

let rec galculator exp =
  let rec get_function exp =
    match exp with
    | X -> fun x -> x
    | INT i -> fun x -> float_of_int i
    | REAL f -> fun x -> f
    | ADD (l, r) -> fun x -> ((get_function l) x) +. ((get_function r) x)
    | SUB (l, r) -> fun x -> ((get_function l) x) -. ((get_function r) x)
    | MUL (l, r) -> fun x -> ((get_function l) x) *. ((get_function r) x)
    | DIV (l, r) -> fun x -> ((get_function l) x) /. ((get_function r) x)
    | SIGMA _ -> fun x -> galculator exp
    | INTEGRAL _ -> fun x -> galculator exp
  in
    let rec sigma (a, b, f) =
      if a > b then
        0.0
      else
        (f a) +. sigma (a +. 1.0, b, f)
    in
      let rec integral (a, b, f) =
        if abs_float (a -. b) < 0.1 then
          0.0 
        else if a > b then
          -. integral (b, a, f)
        else
          ((f a) *. 0.1) +. integral (a +. 0.1, b, f)
      in
        match exp with
        | X -> raise FreeVariable 
        | INT i -> float_of_int i
        | REAL f -> f
        | ADD (l, r) -> (galculator l) +. (galculator r)
        | SUB (l, r) -> (galculator l) -. (galculator r)
        | MUL (l, r) -> (galculator l) *. (galculator r)
        | DIV (l, r) -> (galculator l) /. (galculator r)
        | SIGMA (a, b, f) -> let func = get_function f in
                               sigma (galculator a, galculator b, func)
        | INTEGRAL (a, b, f) -> let func = get_function f in
                                  integral (galculator a, galculator b, func)
