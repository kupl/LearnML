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


let rec toInt : expr -> int = function x ->
match x with
| NUM i -> i
| PLUS (x, y) ->
  let x' = toInt x in
  let y' = toInt y in
 (x' + y')
| MINUS (x, y) ->
  let x' = toInt x in
  let y' = toInt y in
  (x' - y')
  
let rec eval : formula -> bool = function f ->
match f with
| TRUE -> true
| FALSE -> false
| NOT f' -> not (eval f')
| ANDALSO (x, y) -> (eval x) && (eval y)
| ORELSE (x, y) -> (eval x) || (eval y)
| IMPLY (x, y) ->
  let cond = eval x in
  if cond = true then eval y
  else  true
| LESS (x, y) ->
  let x' = toInt x in
  let y' = toInt y in
  (x' < y')
  
  
