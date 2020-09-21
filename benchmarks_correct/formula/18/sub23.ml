type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval : formula -> bool
= fun f -> 
  match f with
    | True -> true
    | False -> false
    | Not(a) -> not(eval(a))
    | AndAlso(b,c) -> eval(b) && eval(c)
    | OrElse(d,e) -> eval(d) || eval(e)
    | Imply(f,g) -> if(eval(f) = false) then true else eval(g)
    | Equal(h,i) -> 
      let rec oper j =
        match j with
          | Num(k) -> k
          | Plus(m,n) -> oper(m) + oper(n)
          | Minus(o,p) -> oper(o) - oper(p) in (oper h = oper i);;
          
eval(Imply(Imply(True,False), True));;
eval(Equal(Num 1, Plus(Num 1, Num 2)));;
