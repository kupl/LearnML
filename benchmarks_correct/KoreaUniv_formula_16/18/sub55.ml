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
= fun f -> (*TODO*)
  let rec cal x= 
    match x with
      | Num a -> a
      | Plus(a,b) -> cal a + cal b
      | Minus(a,b) -> cal a - cal b
  in  
  match f with
  | True -> true
  | False -> false
  | Not a -> not(eval a)
  | AndAlso(a,b) -> eval a && eval b
  | OrElse(a,b) -> eval a || eval b
  | Imply(a,b) -> 
    if (eval a=true&&eval b=false) then false
    else true
  | Equal(a,b) -> cal a = cal b;;
  
eval(Imply(Imply(True,False),True));;
eval(Equal(Num 1,Plus(Num 1,Num 2)));;