type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec sub =
    fun (exp,x) ->
        match (exp,x) with
         | (X,x) -> x 
         | (INT a,x) -> a
         | (ADD(a,b),x) ->(match (a,b) with
                      | (X,b) -> x+ sub (b,x)
                      | (a,X) -> x+ sub (a,x)
                      | (a,b) -> sub (a,x) + sub (b,x))
         | (SUB(a,b),x) -> (match (a,b) with
                      | (X,b) -> x- sub (b,x)
                      | (b,X) -> sub (b,x) - x 
                      | (a,b) -> sub (a,x) - sub (b,x))
         | (MUL(a,b),x) ->(match (a,b) with
                      | (X,b) -> x*(sub(b,x))
                      | (a,X) -> (sub(a,x))*x
                      | (a,b) -> (sub(a,x))*(sub(b,x)))
         | (DIV(a,b),x) -> (match (a,b) with
                      | (X,b) -> x/(sub(b,x))
                      | (a,X) -> (sub(a,x))/x
                      | (a,b) -> (sub(a,x))/(sub(b,x)))
         | (SIGMA(a,b,c),x) ->(match (a,b,c) with
                      | (X,b,c) -> if x = sub(b,x) then sub (c,x) 
                                  else (sub (c,sub (b,x))) + sub(SIGMA(INT x,SUB(b,INT 1),c),x)
                      | (a,X,c) -> if x = sub(a,x) then sub (c,x)
                                  else (sub (c,sub (X,x))) + sub(SIGMA(a,SUB(X,INT 1),c),x) 
                      | (a,b,X) -> if sub(a,x) = sub(b,x) then sub (c,x)
                                  else (sub (X,sub (b,x))) + sub(SIGMA(a,SUB(b, INT 1),X),x)
                      | (a,b,c) -> if (sub (a,x)) = (sub(b,x)) then sub (c,x) 
                                  else (sub (c,sub (b,x))) + sub(SIGMA(a,SUB(b, INT 1),c),sub (c,x)))
  
let rec calculator : exp -> int
= fun exp -> 
  match exp with
  | INT a -> a
  | ADD (a,b) ->(match (a,b) with
                  | (INT a, INT b) -> a+b
                  |_ -> (calculator a) + (calculator b))
  | SUB (a,b) ->(match (a,b) with
                  | (INT a, INT b) -> a-b
                  |_ -> (calculator a) - (calculator b))
  | MUL (a,b) ->(match (a,b) with
                  | (INT a, INT b) -> a * b
                  |_ -> (calculator a) * (calculator b))
  | DIV (a,b) ->(match (a,b) with
                  | (INT a, INT b) -> a / b
                  |_ -> (calculator a) / (calculator b))
  | SIGMA (a,b,c) ->(match (a,b,c) with
                  |(INT a, INT b, c) 
                  -> if a = b then sub (c,b) 
                  else (sub (c,b)) + calculator(SIGMA(INT a ,INT (b-1),c))
                  |(a,b,SIGMA(x,y,z)) 
                  -> if (calculator a) = (calculator b) then sub(SIGMA(x,y,z),(calculator b))
                    else (sub (SIGMA(x,y,z),(calculator b)) + calculator(SIGMA(a,SUB(b,INT 1),SIGMA(x,y,z))))
                  |_-> if (calculator a) = (calculator b) then sub (c,(calculator b)) 
                      else (sub (c,(calculator b)) + calculator(SIGMA(a,SUB(b,INT 1),c))))
