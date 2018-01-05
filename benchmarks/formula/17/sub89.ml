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

let eval : formula -> bool = fun x ->
  match x with
  | TRUE -> true
  | FALSE -> false
  | NOT(y) ->
    (match y with
    |TRUE -> false
    |FALSE -> true
    )
  | ANDALSO(x, y) -> (
    match (x,y) with
    |(TRUE, TRUE) -> true
    |(FALSE, _) -> false
    |(_, FALSE) -> false
    )
  | ORELSE(x,y) -> (
    match (x,y) with
    |(TRUE, _) -> true
    |(_, TRUE) -> true
    |(FALSE,FALSE) -> false
  )
  | IMPLY(x,y) -> (
    match(x,y) with
    |(FALSE, _) -> true
    |(TRUE,TRUE) -> true
    |(TRUE, FALSE) -> false

  )

  | LESS(a,b) -> (
    match (a,b) with
    | (PLUS(NUM(a),NUM(b)), PLUS(NUM(c),NUM(d))) -> (
      if a+b >= c+d then false
      else true
    )
    | (PLUS(NUM(a),NUM(b)), MINUS(NUM(c),NUM(d))) -> (
      if a+b >= c-d then false
      else true
    )
    | (MINUS(NUM(a),NUM(b)), PLUS(NUM(c),NUM(d))) -> (
      if a-b >= c+d then false
      else true
    )
    | (MINUS(NUM(a),NUM(b)), MINUS(NUM(c),NUM(d))) -> (
      if a-b >= c-d then false
      else true
    )
    |(NUM(a),NUM(b)) -> (
    if a >= b then false
    else true
    )

  )
