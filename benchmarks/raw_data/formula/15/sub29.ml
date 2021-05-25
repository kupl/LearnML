(* hw1ex4 "TrueFalse" *)

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


let rec calc (e: exp) : int = 
  match e with
    | Num i -> i
    | Plus (e1,e2) -> (calc e1) + (calc e2)
    | Minus (e1,e2) -> (calc e1) + (calc e2)


let rec eval (f: formula) : bool = 
  match f with
    | True -> true
    | False -> false
    | Not subf -> 
        if (eval subf) = true then false
        else true
    | AndAlso (f1, f2) ->
        if (eval f1) && (eval f2) then true
        else false
    | OrElse (f1, f2) ->
        if (eval f1) || (eval f2) then true
        else false
    | Imply (f1, f2) ->
        if not (eval f1) then true
        else if (eval f2) then true
        else false
    | Equal (e1, e2) ->
        if (calc e1) = (calc e2) then true
        else false

