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

let rec calc (e : expr) : int =
    match e with
    | NUM a -> a
    | PLUS (a, b) -> (calc a) + (calc b)
    | MINUS (a, b) -> (calc a) - (calc b)

let rec eval (f : formula) : bool =
    match f with
    | TRUE -> true
    | FALSE -> false
    | NOT f1 -> not (eval f1)
    | ANDALSO (f1, f2) -> (eval f1) && (eval f2)
    | ORELSE (f1, f2) -> (eval f1) || (eval f2)
    | IMPLY (f1, f2) -> eval (ORELSE (NOT f1, f2))
    | LESS (e1, e2) -> (calc e1) < (calc e2)

