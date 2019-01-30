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
  | Minus of exp * exp;;

let rec ari expr =
  match expr with
    | Num a -> a
    | Plus (c,d) -> (ari c) + (ari d)
    | Minus (c,d) -> (ari c) - (ari d);;

Equal (Num 1, Num 2);;


let rec eval : formula -> bool
= fun f -> 
  match f with
    | True -> true
    | False -> false
    | Not f' -> if eval f' then false
                else true
    | AndAlso (g,h) -> if (eval g && eval h) then true else false
    | OrElse (g,h) -> if (eval g || eval h) then true else false
    | Imply (g,h) -> ( match (eval g,eval h) with |(true, false) ->false | (_ , _)->true)
    | Equal (e,x) -> if ari e = ari x then true else false;;
                 

eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
