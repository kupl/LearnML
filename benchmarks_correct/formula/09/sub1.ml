type formula = True
                 | False
                 | Not of formula
                 | AndAlso of formula * formula
                 | OrElse of formula * formula
                 | Imply of formula * formula
                 | Equal of exp * exp
and exp = Num of int
             | Plus of exp * exp
             | Minus of exp * exp

let rec eval(f : formula) =
  let rec cal(ex : exp) =
     match ex with
          Num a -> a
        | Plus(a,b) -> cal(a) + cal(b)
        | Minus(a,b) -> cal(a) - cal(b) in

    match f with
          True -> true
        | False -> false
        | Not a -> if eval(a)=true then false
                        else true
        | AndAlso(a,b) -> if eval(a)=false then false
                                     else if eval(b)=false then false
                                     else true
        | OrElse(a,b) -> if eval(a)=true then true
                                   else if eval(b)=true then true
                                   else false
        | Imply (a,b) -> if eval(a)=true then
                                    if eval(b)=true then true
                                    else false
                               else true
        | Equal (a,b) -> if (cal(a)=cal(b)) then true
                               else false;;                              