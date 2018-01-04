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

let rec calc x = 
        match x with
        | NUM n -> n
        | PLUS (a, b) -> (calc a) + (calc b)
        | MINUS (a, b) -> (calc a) - (calc b)

let rec eval x = 
        match x with
        | TRUE -> true
        | FALSE -> false
        | NOT form -> not (eval form)
        | ANDALSO (a, b) -> (eval a) && (eval b)
        | ORELSE (a, b) -> (eval a) || (eval b)
        | IMPLY (a, b) -> (not (eval a)) || (eval b)
        | LESS (e_a, e_b) -> (calc e_a) < (calc  e_b)