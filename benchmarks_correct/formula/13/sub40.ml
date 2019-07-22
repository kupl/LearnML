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

let rec eval fm =
  let rec calc expr =
    match expr with
    | NUM n -> n
    | PLUS (expr1, expr2) -> calc expr1 + calc expr2
    | MINUS (expr1, expr2) -> calc expr1 - calc expr2 in
  
  match fm with
  | TRUE -> true
  | FALSE -> false
  | NOT _fm ->
    if eval _fm = true then false else true
  | ANDALSO (fm1, fm2) ->
    if eval fm1 = true && eval fm2 = true then true
    else false
  | ORELSE (fm1, fm2) ->
    if eval fm1 = true || eval fm2 = true then true
    else false
  | IMPLY (fm1, fm2) ->
    if eval fm1 = true && eval fm2 = false then false
    else true
  | LESS (expr1, expr2) ->
    if calc expr1 < calc expr2 then true else false
