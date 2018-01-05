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

let rec galculator expression : float =
  match expression with
  | X -> raise FreeVariable
  | INT n -> float_of_int n
  | REAL r -> r
  | ADD (sub_expression1, sub_expression2) ->
    let value1 = galculator(sub_expression1) in
    let value2 = galculator(sub_expression2) in
    print_float value1;
    print_float value2;
    value1 +. value2
  | SUB (sub_expression1, sub_expression2) ->
    let value1 = galculator(sub_expression1) in
    let value2 = galculator(sub_expression2) in
    value1 -. value2
  | MUL (sub_expression1, sub_expression2) ->
    let value1 = galculator(sub_expression1) in
    let value2 = galculator(sub_expression2) in
    value1 *. value2
  | DIV (sub_expression1, sub_expression2) ->
    let value1 = galculator(sub_expression1) in
    let value2 = galculator(sub_expression2) in
    value1 /. value2
  | SIGMA (a, b, f) ->
    let value_a = galculator a in
    let value_b = galculator b in
    if value_a > value_b then 0.0
    else sum (int_of_float value_a) (int_of_float value_b) f
  | INTEGRAL (a, b, f) ->
    let value_a = galculator a in
    let value_b = galculator b in
    if value_a < value_b then integrate value_a value_b f 0.1
    else -. (integrate value_b value_a f 0.1)
and evaluate expression xvalue : float =
  match expression with
  | X -> xvalue
  | INT n -> float_of_int n
  | REAL r -> r
  | ADD (sub_expression1, sub_expression2) ->
    let value1 = evaluate sub_expression1 xvalue in
    let value2 = evaluate sub_expression2 xvalue in
    value1 +. value2
  | SUB (sub_expression1, sub_expression2) ->
    let value1 = evaluate sub_expression1 xvalue in
    let value2 = evaluate sub_expression2 xvalue in
    value1 -. value2
  | MUL (sub_expression1, sub_expression2) ->
    let value1 = evaluate sub_expression1 xvalue in
    let value2 = evaluate sub_expression2 xvalue in
    value1 *. value2
  | DIV (sub_expression1, sub_expression2) ->
    let value1 = evaluate sub_expression1 xvalue in
    let value2 = evaluate sub_expression2 xvalue in
    value1 /. value2
  | SIGMA (a, b, f) ->
    let value_a = galculator a in
    let value_b = galculator b in
    if value_a > value_b then 0.0
    else sum (int_of_float value_a) (int_of_float value_b) f
  | INTEGRAL (a, b, f) ->
    let value_a = galculator a in
    let value_b = galculator b in
    if value_a < value_b then integrate value_a value_b f 0.1
    else -. (integrate value_b value_a f 0.1)
and integrate (a : float) (b : float) (f : exp) (dx: float) : float =
  if a > b then 0.0
  else
    let realdx =
      if b -. a < dx then  (b -. a)
      else dx
    in
    let smallarea = (evaluate f a) *. realdx
    in
    smallarea +. (integrate (a +. dx) b f dx)
and sum (a : int) (b : int) (f : exp) : float =
  if a > b then 0.0
  else
    let addup = (evaluate f (float_of_int a))
    in
    addup +. (sum (a+1) b f);;
