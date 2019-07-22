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

let rec cal e = 
  match e with
    | Num(e1) -> e1
    | Plus(e1,e2) -> (cal e1)+(cal e2)
    | Minus(e1,e2) -> (cal e1)-(cal e2);;

let rec eval f =
  match f with
    | True -> true
    | False -> false
    | Not(f1) -> 
      if (eval f1)==true then false
      else true
    | AndAlso(f1,f2) ->
      if (eval f1)==true && (eval f2)==true then true 
      else false
    | OrElse(f1,f2) -> 
      if (eval f1)==false && (eval f2)==false then false 
      else true
    | Imply(f1,f2) -> 
      if (eval f1)==true && (eval f2)==false then false 
      else true 
    | Equal(e1,e2) -> 
      if (cal e1)==(cal e2) then true 
      else false;;