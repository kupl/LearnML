type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
=fun e ->
  let rec sigma f l func = 
      if f <= l then (func f) + (sigma (f+1) l func) 
      else 0 
  in let rec exp_to_func : exp -> (int -> int) 
  =fun input ->
    match input with
    | X -> (fun a -> a)
    | INT (i) -> (fun a -> i) 
    | ADD (x, y) -> (fun a -> ((exp_to_func x) a) + ((exp_to_func y) a))  
    | SUB (x, y) -> (fun a -> ((exp_to_func x) a) - ((exp_to_func y) a))
    | MUL (x, y) -> (fun a -> ((exp_to_func x) a) * ((exp_to_func y) a))
    | DIV (x, y) -> (fun a -> ((exp_to_func x) a) / ((exp_to_func y) a))
  in match e with
  | INT (i) -> i 
  | ADD (INT x, INT y) -> x + y   
  | SUB (INT x, INT y) -> x - y 
  | MUL (INT x, INT y) -> x * y 
  | DIV (INT x, INT y) -> x / y 
  | SIGMA (INT (x), INT (y), z) -> sigma x y (exp_to_func z)
