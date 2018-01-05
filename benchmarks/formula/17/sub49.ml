
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

let rec eval : formula -> bool = 
	let rec expr_to_int : expr -> int = fun expr ->
	match expr with
	| NUM a -> a
	| PLUS (a, b) -> expr_to_int(a) + expr_to_int(b)
	| MINUS (a, b) -> expr_to_int(a) - expr_to_int(b) in
fun formula ->
match formula with
| TRUE -> true
| FALSE -> false
| NOT a -> 
	if(eval a) then false
	else true
| ANDALSO (a, b) -> 
	if(eval a) then eval b
	else false
| ORELSE (a, b) ->
	if(eval a) then true
	else eval b
| IMPLY (a, b) ->
	if(eval a) then eval b
	else true
| LESS (a, b) -> expr_to_int(a) < expr_to_int(b)


