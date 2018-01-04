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

let rec eval (f: formula) =
  let rec calc (e: expr) =
    match e with
    | NUM n -> n
    | PLUS (a, b) -> (calc a) + (calc b)
    | MINUS (a, b) -> (calc a) - (calc b)
  in
  match f with
    | TRUE -> true
    | FALSE -> false
    | NOT f' -> not (eval f')
    | ANDALSO (f', f'') -> (eval f') && (eval f'')
    | ORELSE (f', f'') -> (eval f') || (eval f'')
    | IMPLY (f', f'') -> not ((eval f') && (not (eval f'')))
    | LESS (e, e') -> (calc e) < (calc e')    