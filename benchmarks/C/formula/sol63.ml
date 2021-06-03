type exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp


    
let rec eval f =
    let rec val_of_exp exp = 
        match exp with
        Num a -> a
        | Plus (a,b) -> (val_of_exp a)+(val_of_exp b)
        | Minus (a,b) -> (val_of_exp a)-(val_of_exp b)
    in
        
    match f with
    True -> true
    | False -> false
    | Not a -> not (eval a)
    | AndAlso (a,b) -> (eval a)&(eval b)
    | OrElse (a,b) -> (eval a)||(eval b)
    | Imply (a,b) -> (not (eval a))||(eval b)
    | Equal (e1,e2) -> (val_of_exp e1)=(val_of_exp e2)
      
