exception FreeVariable

type exp =
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

(*Priority of the inner X is higher than outer X*)
(*Think about general program*)

let rec calculator : exp -> exp -> float = 
  fun currX e -> match e with 
  | X -> (match currX with
        | X -> raise FreeVariable
        | _ -> (calculator X currX))
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (calculator currX e1)+.(calculator currX e2)
  | SUB (e1, e2) -> (calculator currX e1)-.(calculator currX e2)
  | MUL (e1, e2) -> (calculator currX e1)*.(calculator currX e2)
  | DIV (e1, e2) -> (calculator currX e1)/.(calculator currX e2)
  | SIGMA (fromX, toX, e_in) -> 
     let fromXCalculated = int_of_float (calculator currX fromX) 
     in
     let toXCalculated = int_of_float (calculator currX toX) 
     in
     let calWithX = (calculator (INT fromXCalculated))
     in
     let calWithOutX = (calculator currX)
     in 
     if (fromXCalculated > toXCalculated) 
       then 0.0
     else if (fromXCalculated=toXCalculated) then (calWithX e_in)  
     else (calWithX e_in) +. (calWithOutX (SIGMA (INT (fromXCalculated+1),(INT toXCalculated), e_in)))
  | INTEGRAL (fromX, toX, e_in) -> 
     let fromXCalculated = (calculator currX fromX) 
     in
     let toXCalculated = (calculator currX toX) 
     in
     let calWithX = (calculator (REAL fromXCalculated))
     in 
     let calWithOutX = (calculator currX)
     in
     if (fromXCalculated > toXCalculated) 
       then (calWithOutX (MUL (REAL (-1.0), (INTEGRAL (REAL toXCalculated, REAL fromXCalculated, e_in)))))
     else if (abs_float (fromXCalculated-.toXCalculated)) < 0.1
       then 0.0  
     else 0.1*.(calWithX e_in) 
          +.(calWithOutX (INTEGRAL (REAL (fromXCalculated+.0.1),(REAL toXCalculated), e_in)))

let galculator : exp -> float = 
  fun expr -> (calculator X expr)
