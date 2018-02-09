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

let rec eval formula=
match formula with
| True-> true
| False-> false
| Not(x)->
if eval x=false then true
else false
| AndAlso(x,y) ->
if eval x=true&&eval y=true then true
else if eval x=true&&eval y=false then false
else if eval x=false&&eval y=true then false
else false
| OrElse(x,y)->
if eval x=true&&eval y=true then true
else if eval x=true&&eval y=false then true
else if eval x=false&&eval y=true then true
else false
| Imply(x,y)->
if eval x=true&&eval y=true then true
else if eval x=true&&eval y=false then false
else if eval x=false&&eval y=true then true
else true
| Equal(x,y)->
let rec exp fxy=
match fxy with
| Num n->n
| Plus(x,y)->exp x+exp y
| Minus(x,y)->exp x-exp y
in
if exp x==exp y then true
else false
;;
