type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp
and exp = Num of int
         | Plus of exp * exp
         | Minus of exp * exp


let rec toInt : exp -> int = function x ->
match x with
| Num i -> i
| Plus (x, y) ->
  let x' = toInt x in
  let y' = toInt y in
 (x' + y')
| Minus (x, y) ->
  let x' = toInt x in
  let y' = toInt y in
  (x' - y')
  
let rec eval : formula -> bool = function f ->
match f with
| True -> true
| False -> false
| Not f' -> not (eval f')
| AndAlso (x, y) -> (eval x) && (eval y)
| OrElse (x, y) -> (eval x) || (eval y)
| Imply (x, y) ->
  let cond = eval x in
  if cond = true then eval y
  else  true
| Equal (x, y) ->
  let x' = toInt x in
  let y' = toInt y in
  (x' = y')
  
  
