type formula = True
             | False
             | Not of formula
             | AndAlso of formula*formula
             | OrElse of formula*formula
             | Imply of formula*formula
             | Equal of exp*exp
and exp = Num of int
         | Plus of exp*exp
         | Minus of exp*exp
let rec exp_value exp =
    match exp with
      Num(n) -> n
    | Plus(e1,e2) -> exp_value(e1) + exp_value(e2)
    | Minus(e1,e2) -> exp_value(e1) - exp_value(e2)
let rec eval formula =
    match formula with
      True -> true
    | False -> false
    | Not(f1) -> not(eval(f1))
    | AndAlso(f1,f2) -> eval(f1) & eval(f2)
    | OrElse(f1,f2) -> eval(f1) || eval(f2)
    | Imply(f1,f2) -> if eval(f1)=false || eval(f2)=true then true               
                      else false
    | Equal(e1,e2) -> exp_value(e1) = exp_value(e2)
