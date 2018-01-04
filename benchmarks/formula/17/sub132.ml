type formula = TRUE
|FALSE
|NOT of formula
|ANDALSO of formula * formula
|ORELSE of formula * formula
|IMPLY of formula * formula
|LESS of expr * expr
and expr = NUM of int
|PLUS of expr * expr
|MINUS of expr * expr

let rec eval : formula -> bool = fun form ->
match form with
|TRUE -> true
|FALSE -> false
|NOT form -> not (eval form)
|ANDALSO (f1,f2) -> if eval f1 && eval f2 then true else false
|ORELSE (f1,f2) -> if eval f1 || eval f2 then true else false
|IMPLY (f1,f2) -> if eval f1 && not (eval f2) then false else true
|LESS (e1,e2) -> if calc e1 < calc e2 then true else false
and calc : expr -> int = fun ex ->
match ex with
|NUM integr -> integr
|PLUS (e1,e2) -> calc e1 + calc e2
|MINUS (e1,e2) -> calc e1 - calc e2
