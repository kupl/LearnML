type formula =
  True
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

let rec simpl e =
  match e with
    Num n -> n
  | Plus (se1, se2) -> simpl se1 + simpl se2
  | Minus (se1, se2) -> simpl se1 - simpl se2

let rec eval : formula -> bool =
  fun f ->
    match f with
      True -> true
    | False -> false
    | Not sf -> not (eval sf)
    | AndAlso (sf1, sf2) -> eval sf1 && eval sf2
    | OrElse (sf1, sf2) -> eval sf1 || eval sf2
    | Imply (sf1, sf2) -> eval sf1 <= eval sf2
    | Equal (exp1, exp2) -> simpl exp1 = simpl exp2;;
    
eval (Imply (Equal (Minus (Plus (Num 1, Num 4), Num 5), Num 6), AndAlso (Equal (Num 5, Num 7), OrElse (True, False))));;
eval (Equal (Plus (Num 1, Num 4), Num 5));;
eval (Imply (Imply (True, False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
