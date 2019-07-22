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

let rec eval f =
  let rec calc exp =
    match exp with
      | NUM x -> x
      | PLUS (e0, e1) -> (calc e0) + (calc e1)
      | MINUS (e0, e1) -> (calc e0) - (calc e1)
  in
    match f with
      | TRUE -> true
      | FALSE -> false
      | NOT f0 -> not (eval f0)
      | ANDALSO (f0, f1) -> (eval f0) && (eval f1)
      | ORELSE (f0, f1) -> (eval f0) || (eval f1)
      | IMPLY (f0, f1) -> (not (eval f0)) || (eval f1)
      | LESS (e0, e1) -> (calc e0) < (calc e1)
