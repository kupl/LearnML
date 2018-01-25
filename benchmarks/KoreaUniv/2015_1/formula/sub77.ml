type formula =
True
| False
| Neg of formula
| Or of formula * formula
| And of formula * formula
| Imply of formula * formula
| Equiv of formula * formula;;

let eval : formula -> bool = fun f -> true;;

let eval f =
	match f with
	True -> true
| False -> false
| Neg a -> if a = True then false else true
| Or (a,b) ->
    if a = True then true
    else if b = True then true
    else false
| And (a,b) ->
		if a = False then false
    else if b = False then false
    else true
| Imply (a,b) ->
    if a = False then true
    else if  b = True then true
    else false
| Equiv (a,b) ->
    if a = True then (if b = True then true else false)
    else (if b = False then true else false)
