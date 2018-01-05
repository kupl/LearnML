type expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr

type formula = TRUE
    | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr

let rec getnum e : int = 
    match e with
    | NUM a -> a
    | PLUS (a, b) -> (getnum a) + (getnum b)
    | MINUS (a, b) -> (getnum a) - (getnum b)

let rec eval f : bool =
    match f with
    | TRUE -> true
    | FALSE -> false
    | NOT a -> not (eval a)
    | ANDALSO (a, b) -> (eval a) && (eval b)
    | ORELSE (a, b) -> (eval a) || (eval b)
    | IMPLY (a, b) when ((eval a) = true && (eval b) = false) -> false
    | IMPLY (a, b) -> true
    | LESS (a, b) -> (getnum a) < (getnum b)

(*let a61 = eval TRUE
let a62 = eval FALSE
let a63 = eval (NOT TRUE)
let a64 = eval (ANDALSO (TRUE, FALSE))
let a65 = eval (ORELSE (TRUE, FALSE))
let a66 = eval (LESS (PLUS(NUM 3, NUM 4), MINUS(NUM 7, NUM 8))) 

let _ = print_endline(string_of_bool a61)
let _ = print_endline(string_of_bool a62)
let _ = print_endline(string_of_bool a63)
let _ = print_endline(string_of_bool a64)
let _ = print_endline(string_of_bool a65)
let _ = print_endline(string_of_bool a66)*)
