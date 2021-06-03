type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
and exp = Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec count a = 
  match a with
    |Num n -> n
    |Plus (m,n) -> count m + count n
    |Minus (m,n) -> count m - count n

let rec eval a =
  match a with
    |True -> true
    |False -> false
    |Not nota -> not(eval nota)
    |AndAlso (aa, ab) -> eval aa && eval ab
    |OrElse (aa, ab) -> eval aa || eval ab
    |Imply (aa, ab) -> 
      if(eval aa && not(eval ab)) then false else true 
    |Equal (expm, expn) -> 
      if(count(expm) = count(expn)) then true else false

