
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

 
let rec cal =
   fun (exp, x)->
      match (exp, x) with
      | (X,x)->x
      | (INT a,x) -> a
      | (ADD (a,b), x) -> (match (a, b) with 
                     | (X, b)-> x + cal (b,x)
                     | (a, X)-> x + cal (a,x)
                     | (a, b)-> cal (a,x) + cal(b,x)) 
      | (SUB (a,b), x) -> (match (a, b) with
                     | (X, b)-> x - cal (b,x)
                     | (a, X)-> x - cal (a,x)
                     | (a, b)-> cal (a,x) - cal(b,x)) 
      | (MUL (a,b), x) -> (match (a, b) with
                     | (X, b)-> x * cal (b,x)
                     | (a, X)-> x * cal (a,x)
                     | (a, b)-> cal (a,x) * cal(b,x)) 
      | (DIV (a,b), x) -> (match (a, b) with
                     | (X, b)-> x / cal (b,x)
                     | (a, X)-> x / cal  (a,x)
                     | (a, b)-> cal (a,x) / cal (b,x)) 
      | (SIGMA (a,b,c), x)->(match a,b,c with
                     | X, b, c-> if x= cal(b,x) then cal (c,x)
                                 else cal (c, (cal (b,x))) + cal (SIGMA (INT x, SUB(b, INT 1),c),x)
                     | a, X, c-> if x= cal (a,x) then cal (c,x)
                                 else cal( c, (cal (X, x))) + cal (SIGMA (a, SUB(X, INT 1),c),x)
                     | a, b, X-> if cal (a,x) = cal (b,x)  then cal (b,x)
                                 else cal (X, cal(b,x))+ cal (SIGMA (a, SUB(b, INT 1),X),x))

  let rec calculator : exp -> int
  = fun exp -> 
  match exp with 
  | X-> raise (Failure "can't be calculated") 
  | (INT n)-> n
  | (ADD (a, b))-> (match a,b with 
                  | INT a, INT b-> a+b
                  | INT a, b-> a+ (calculator b)
                  | a, INT b -> (calculator a) + b
                  | a , b -> (calculator a) + (calculator b))
  |( SUB (a, b)) ->(match a,b with
                  | INT a, INT b-> a - b 
                  | INT a, b-> a - (calculator b) 
                  | a, INT b -> (calculator a) - b
                  | a , b -> (calculator a) - (calculator b))  
  | (MUL (a, b))-> (match a,b with
                  | INT a, INT b-> a * b
                  | INT a, b-> a * (calculator b) 
                  | a, INT b -> (calculator a) *  b
                  | a , b -> (calculator a) * (calculator b))
  | (DIV (a, b)) -> (match a,b with
                  | INT a, INT b-> a / b
                  | INT a, b-> a / (calculator b) 
                  | a, INT b -> (calculator a) / b
                  | a , b -> (calculator a) / (calculator b))
  | (SIGMA (a,b,c))-> (match a,b,c with 
                  | INT a, INT b, c -> if a=b then cal (c,b) else (cal (c,b))+ calculator (SIGMA (INT a, INT (b-1),c ))
                  | a,b, SIGMA (x,y,z) -> if (calculator a) = (calculator b) then cal (SIGMA (x,y,z), (calculator b)) else cal (SIGMA( x,y,z), (calculator b) ) + calculator( SIGMA (a, SUB (b, INT 1), SIGMA ( x,y,z))) 
                  |_-> if (calculator a) = (calculator b) then cal (c,(calculator b)) else (cal(c,(calculator b))+ calculator (SIGMA (a, SUB( b, INT 1),c))))
