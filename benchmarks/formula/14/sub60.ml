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

let rec parse e =
match e with
| NUM(n) -> n
| PLUS(e1,e2) -> parse e1 + parse e2
| MINUS(e1,e2) -> parse e1 - parse e2

let rec eval f =
match f with
| TRUE -> true
| FALSE -> false
| NOT(f) -> not (eval f)
| ANDALSO(a,b) -> (eval a)&&(eval b)
| ORELSE(a,b) -> (eval a)||(eval b)
| IMPLY(a,b) -> if (eval a)=true then eval b else true
| LESS(e1,e2) -> parse e1 < parse e2

