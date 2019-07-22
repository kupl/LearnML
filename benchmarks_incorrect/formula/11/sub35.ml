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
let rec cal f=match f with
        |NUM(n)->n
        |PLUS(a,b)->cal(a)+cal(b)
        |MINUS(a,b)->cal(a)-cal(b)
let rec eval f=match f with
        |TRUE->true
        |FALSE->false
        |NOT(a)->if eval(a)=true then false else true 
        |ANDALSO(a,_)->eval(a)
        |ORELSE(a,b)->if eval(a)=true then true else eval(b)
        |IMPLY(a,b)->if eval(a)=false then true else eval(b)
        |LESS(a,b)->if cal(a)<cal(b) then true else false
