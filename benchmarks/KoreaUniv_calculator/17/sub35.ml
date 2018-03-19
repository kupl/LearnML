(*problem 5*) 
  type exp = X
    |INT of int
    |ADD of exp *exp
    |SUB of exp *exp
    |MUL of exp *exp
    |DIV of exp *exp
    |SIGMA of exp *exp *exp

    
       let rec change:exp *int ->exp
       =fun(e,k) ->
          match e with 
          |X -> INT k
          |INT n -> INT n
          |ADD(a,b) -> ADD(change(a,k),change(b,k))
          |SUB(a,b) -> SUB(change(a,k),change(b,k))
          |MUL(a,b) -> MUL(change(a,k),change(b,k))
          |DIV(a,b) -> DIV(change(a,k),change(b,k))
          |SIGMA(a,b,c) -> change(c,k)

     let find: exp->int 
     = fun e->
        let rec calculate: exp-> int
        =fun e->
          match e with
          |X->1
          |INT n->n
          |ADD(a,b) -> calculate(a)+calculate(b)
          |SUB(a,b) -> calculate(a)-calculate(b)
          |MUL(a,b) -> calculate(a)*calculate(b)
          |DIV(a,b) -> calculate(a)/calculate(b)
          |SIGMA(a,b,c)-> if calculate(a)>calculate(b) then 0 
   else calculate(change(SIGMA(a,b,c),calculate(a)))+calculate(SIGMA(ADD(a,INT 1),b,c))
    in calculate(e)
                          
;;