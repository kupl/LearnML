type formula = TRUE
            |FALSE
            |NOT of formula
            |ANDALSO of formula * formula
            |ORELSE of formula * formula
            |IMPLY of formula * formula
            |LESS of expr * expr
    and expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr

let rec calc e =
    match e with
    |NUM i -> i
    |PLUS (a, b) -> (calc a) + (calc b)
    |MINUS (a, b) -> (calc a) - (calc b)

let rec eval f =
    match f with
    |TRUE -> true
    |FALSE -> false
    |NOT a -> eval a
    |ANDALSO (a, b) -> if (eval a) = true then eval b
                        else false
    |ORELSE (a, b) -> if (eval a) = false then eval b
                        else true
    |IMPLY (a, b) -> if (eval a) = true then eval b
                        else true
    |LESS (a, b) -> if (calc a) < (calc b) then true
                        else false

