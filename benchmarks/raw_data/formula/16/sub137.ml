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
   | Not form -> if eval form = true then
        false
      else
        true
   | AndAlso (form1, form2) ->
      if eval form1 = true && eval form2 = true then
        true
      else
        false
   | OrElse (form1, form2) ->
      if eval form1 = false && eval form2 = false then
        false
      else
        true
   | Imply (form1, form2) ->
      if eval form1 = true && eval form2 = false then
        false
      else
        true
   | Equal (ex1, ex2) ->
   	let rec eti e =
   	match e with
       |Num n -> n
       |Plus (n, l) -> (eti n) + (eti l)
       |Minus (n, l) -> (eti n) - (eti l) in
        if (eti ex1) = (eti ex2) then
          true
        else
          false;;
