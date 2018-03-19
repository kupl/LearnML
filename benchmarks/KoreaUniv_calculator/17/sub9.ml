(*Problem 5*)
 type exp = X
 |INT  of int 
 |ADD of exp * exp
 |SUB of exp * exp
 |MUL of exp * exp 
 |DIV of exp * exp
 |SIGMA of exp * exp *exp

 let rec sigf f x = 
 match f with 
 |X -> (match x with 
     |INT (n) -> n
     |_ -> raise (Failure "It must be INT"))
 
 |INT n -> n
 |ADD (a,b) -> (sigf a x) + (sigf b x)
 |SUB (a,b) -> (sigf a x) - (sigf b x)
 |MUL (a,b) -> (sigf a x) * (sigf b x)
  |DIV (a,b) -> (sigf a x) / (sigf b x)

  let rec sigma (a,b,c) =
   if (sigf a a) < (sigf b b) then (sigf c a) + sigma(INT((sigf a a)+1),b,c) else (sigf a c)

       let rec calculator: exp -> int
       = fun e -> (match e with 
         |INT (num) -> num 
         |ADD (a,b) -> calculator(a) + calculator (b)
         |SUB (a,b) -> calculator(a) - calculator (b)
         |MUL (a,b) -> calculator(a) * calculator (b) 
         |DIV (a,b) -> calculator(a) / calculator (b) 
         |SIGMA (a,b,c) -> (match c with 
           |SIGMA(x,y,z) -> (calculator(b) - calculator(a)+1)*sigma(x,z,y)
           |_ -> sigma(a,b,c)))