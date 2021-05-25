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
  
let rec chlwhd : exp -> int
= fun e ->
  match e with
      Num a -> a
    | Plus (a, b) -> chlwhd(a) + chlwhd(b)
    | Minus (a, b) -> chlwhd(a) - chlwhd(b);;

let rec eval : formula -> bool
= fun f -> 
  match f with 
    True -> true
  | False -> false
  | Not f -> not (eval (f)) 
  | AndAlso (f', f) -> eval (f') && eval (f)
  | OrElse (f', f) -> eval (f') || eval (f)
  | Imply (f', f) -> if eval (f') = true && eval (f) = false then false
                    else if eval (f') = true && eval (f) = true then true
                    else if eval (f') = false && eval (f) = true then true
                    else false
  | Equal (a, b) -> if chlwhd (a) = chlwhd(b) then true
                    else false;;
                    

(*
Write the function
  eval : formula -> bool
that computes the truth value of a given formula. For example,
  eval (Imply (Imply (True,False), True))
evaluates to true, and
  eval (Equal (Num 1, Plus (Num 1, Num 2)))
evaluates to false. *)
