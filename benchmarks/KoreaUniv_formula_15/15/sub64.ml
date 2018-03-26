type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f ->
match f with
  True -> true 
| False -> false
| Neg f -> 
if (eval f) = true then false 
else true
| Or (f1, f2) ->
if (eval f1) = true then true
else (eval f2)
| And (f1, f2) ->
if (eval f1) = false then false 
else (eval f2)
| Imply (f1, f2) ->
if (eval f2) = true then true 
else if (eval f1) = false then true
else false
| Equiv (f1, f2) ->
(eval f1) = (eval f2);;
