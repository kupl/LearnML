
(*real code start*)
type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and  expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

let rec calc (e: expr) : int =
match e with
|NUM(x) -> x
|PLUS(x,y) -> calc(x) + calc(y)
|MINUS(x,y) -> calc(x) - calc(y)

let rec eval (f: formula) : bool =
match f with
|TRUE -> true
|FALSE -> false
|NOT(f1) -> if(eval(f1)==true) then false else true
|ANDALSO(f1,f2) -> if(eval(f1) == true && eval(f2) == true) then true else false
|ORELSE(f1,f2) -> if(eval(f1)==false && eval(f2) == false ) then false else true
|IMPLY(f1,f2) -> if(eval(f1)==false || eval(f2) == true ) then true else false
|LESS(e1, e2) -> if(calc(e1) < calc(e2)) then true else false
(*real code end*)
