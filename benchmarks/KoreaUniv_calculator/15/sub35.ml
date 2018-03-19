type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec subcal1 : exp*exp->int
  = fun (n, exp) ->
    match exp with
    | X -> subcal1(n,n)
    | INT(a) -> a
    | ADD(x, y) -> subcal1(n,x) + subcal1(n,y)
    | SUB(x, y) -> subcal1(n,x) - subcal1(n,y)
    | MUL(x, y) -> subcal1(n,x) * subcal1(n,y)
    | DIV(x, y) -> subcal1(n,x) / subcal1(n,y)

let rec sigma : exp*exp*exp-> int
  = fun(a,b,f)->
    if subcal1(a,a) > subcal1(b,b) then subcal1(a,f)+sigma(INT(subcal1(a,a)),b,f)
    else subcal1(a,f)

let rec subcal2 exp =
  match exp with
  |INT(a) -> a
  |ADD(x,y) -> subcal2(x)+subcal2(y)
  |SUB(x,y) -> subcal2(x)-subcal2(y)
  |MUL(x,y) -> subcal2(x)*subcal2(y)
  |DIV(x,y) -> subcal2(x)/subcal2(y)
  |SIGMA(a,b,f) ->match f with
                  |SIGMA(x,y,g)->(subcal2(b)-subcal2(a)+1)*sigma(x,y,g)
                  |_->sigma(a,b,f)

let calculator e = subcal2(e);;  