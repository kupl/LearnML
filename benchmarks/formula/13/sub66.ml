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
  let rec eve e =
  match e with
  NUM n -> n
|  PLUS (e1, e2) -> eve e1 + eve e2
|  MINUS (e1, e2) -> eve e1 - eve e2
  let rec eval f =
  match f with
  TRUE -> true
|  FALSE -> false
|  NOT f -> not(eval f)
  | ANDALSO (f1, f2)-> if(eval f1 && eval f2) then true else false  
| ORELSE (f1, f2) -> if(eval f1 || eval f2) then true else false  
| IMPLY (f1, f2) -> if(eval f1 && eval f2) then true else if (not(eval f1)) then true else false
  | LESS (e1, e2) ->   if(eve e1 < eve e2) then true else false
