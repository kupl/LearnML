type formula = True | False | Neg of formula | Or of formula * formula | And of formula * formula | Imply of formula * formula | Equiv of formula * formula 
let rec eval f =
match f with
True -> true
|False -> false
|Neg f -> 
if x=True then true then false else true
|Or (x,y) -> 
if x = True then true else if y=True then true else false
|And (x,y) ->
if x=True then eval y else false 
|Imply (x,y) ->
if x = y then false else true 
|Equiv(x,y) ->
if x = y then true else false;;
