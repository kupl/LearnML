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

let rec eval = (fun x -> 
	let rec evalin = (fun x -> (match x with
| PLUS (a, b) -> (evalin a) + (evalin b)
| MINUS (a, b) -> (evalin a) - (evalin b)
| NUM a -> a
	)) in	
	(match x with
| FALSE -> false
| TRUE -> true
| ANDALSO (a, b) -> (eval a) && (eval b)
| ORELSE (a, b) -> (eval a) || (eval b)
| IMPLY (a, b) -> ((eval a) = false) || ((eval b) = true)
| LESS (a, b) -> (evalin a) < (evalin b)
| NOT a -> ((eval a) = false)
 ) 
);;
