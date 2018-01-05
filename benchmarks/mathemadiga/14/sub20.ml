exception FreeVariable

type exp = X
         | INT of int
         | REAL of float
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
         | INTEGRAL of exp * exp * exp

let rec galculator (e: exp) : float = 
  match e with
  | X -> raise FreeVariable
  | INT i -> float_of_int(i)
  | REAL f -> f
  | ADD (e1, e2) -> (galculator e1) +. (galculator e2)
  | SUB (e1, e2) -> (galculator e1) -. (galculator e2)
  | MUL (e1, e2) -> (galculator e1) *. (galculator e2)
  | DIV (e1, e2) -> (galculator e1) /. (galculator e2)

  | SIGMA (e1, e2, e3) -> if (int_of_float (galculator e1)) > (int_of_float (galculator e2)) then 0.
		          else let ee1 = INT (int_of_float (galculator e1)) in
                               (galculator (applyVal e3 ee1)) +. (galculator (SIGMA (ADD (INT 1, ee1), e2, e3)))

  | INTEGRAL (e1, e2, e3) -> if (galculator e1) > (galculator e2) then (galculator (SUB (REAL 0., INTEGRAL(e2, e1, e3))))
                             else if (abs_float ((galculator e1) -. (galculator e2))) < 0.1 then 0.
			     else let ee1 = REAL (galculator e1) in
				  ((galculator (applyVal e3 ee1)) *. 0.1) +. (galculator (INTEGRAL (ADD (REAL 0.1, ee1), e2, e3 )))

and  applyVal (e_func: exp) (e_val: exp): exp = 
  match e_func with
       | X -> e_val
       | INT i -> INT i
       | REAL f -> REAL f
       | ADD (e1, e2) -> ADD ( (applyVal e1 e_val), (applyVal e2 e_val) )
       | SUB (e1, e2) -> SUB ( (applyVal e1 e_val), (applyVal e2 e_val) )
       | MUL (e1, e2) -> MUL ( (applyVal e1 e_val), (applyVal e2 e_val) )
       | DIV (e1, e2) -> DIV ( (applyVal e1 e_val), (applyVal e2 e_val) )
       | SIGMA (e1, e2, e3) -> REAL (galculator (SIGMA (e1, e2, e3)))
       | INTEGRAL (e1, e2, e3) -> REAL (galculator (INTEGRAL (e1, e2, e3)))

