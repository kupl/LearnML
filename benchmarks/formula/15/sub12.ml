
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

let rec calc : expr -> int = fun e ->
    match e with
    | NUM i -> i
    | PLUS (e1, e2) -> calc e1 + calc e2
    | MINUS (e1, e2) -> calc e1 - calc e2

let rec eval : formula -> bool = fun f -> 
   match f with
   | TRUE -> true
   | FALSE -> false
   | NOT f1 -> not (eval f1)
   | ANDALSO (f1, f2) -> (eval f1) && (eval f2)
   | ORELSE (f1, f2) -> (eval f1) || (eval f2)
   | IMPLY (f1, f2) -> if eval f1 == true && eval f2 == false then false
                       else true
   | LESS (e1, e2) -> if calc e1 < calc e2 then true
                      else false

(*

let _ = print_endline (string_of_bool (eval TRUE))
let _ = print_endline (string_of_bool (eval FALSE))
let _ = print_endline (string_of_bool (eval(NOT TRUE)))
let _ = print_endline (string_of_bool (eval(ANDALSO(TRUE, FALSE))))
let _ = print_endline (string_of_bool (eval(ORELSE(TRUE, FALSE))))
let _ = print_endline (string_of_bool (eval(IMPLY(TRUE, FALSE))))

let a = NUM 10
let b = NUM 15
let c = PLUS(a, b)
let d = MINUS(a, b)

let _ = print_endline (string_of_bool (eval(LESS(a, b))))
let _ = print_endline (string_of_bool (eval(LESS(c, b))))
let _ = print_endline (string_of_bool (eval(LESS(d, b))))

*)
