 type exp = X
           | INT of int
           | ADD of exp * exp
           | SUB of exp * exp
           | MUL of exp * exp
           | DIV of exp * exp
           | SIGMA of exp * exp * exp
  let calculator : exp -> int
  =fun e -> 
    let rec cal : exp-> int
    = fun e ->(
    match e with
    |X -> 0
    |INT n -> n
    |ADD (a,b) -> (cal a)+(cal b)
    |SUB (a,b) -> (cal a)-(cal b)
    |MUL (a,b) -> (cal a)*(cal b)
    |DIV (a,b) -> (cal a)/(cal b)
    |SIGMA (a,b,c) -> 
      (let rec func : exp * int -> int
      = fun (n,i) -> match n with
       |X-> i
       |INT n -> n
       |ADD (x,y) -> (func(x,i)) + (func(y,i))
       |SUB (x,y) -> func(x,i) - func(y,i)
       |MUL (x,y) -> func(x,i) * func(y,i)
       |DIV (x,y) -> func(x,i) / func(y,i)
       |_ ->i
      in
       if a<=b then cal( ADD( INT(func (c , (cal a))) , SIGMA( INT( cal(a) + 1 ), b, c ) ) ) else 0) )
  in
  cal e;;



