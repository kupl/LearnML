(*2011-11004 ³²À±¼® ¹®Á¦2*)

type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec cal n =
	match n with
		NUM x -> x
		|PLUS(x, y) -> cal x + cal y
		|MINUS(x, y) -> cal x - cal y


let rec eval b =
	match b with
		TRUE -> true
		| FALSE -> false
		| NOT x -> 
			if eval x = true then false
			else true
		| ANDALSO (x, y) ->
			if eval x = true && eval y = true then true
			else false
		| ORELSE (x, y) ->
			if eval x = true || eval y = true then true
			else false
		| IMPLY (x, y) ->
			if eval x = true && eval y = false then false
			else true
		| LESS (x, y) ->
			if cal x < cal y then true
			else false

