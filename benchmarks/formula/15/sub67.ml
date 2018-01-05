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

let rec calc e =
    match e with
    | NUM (n) -> n
    | PLUS (n, m) -> calc(n) + calc(m)
    | MINUS (n, m) -> calc(n) - calc(m);;

let rec eval f =
    match f with
    | TRUE -> true
    | FALSE -> false
    | NOT p -> not (eval p)
    | ANDALSO (p, q) -> (eval p) && (eval q)
    | ORELSE (p, q) -> (eval p) || (eval q)
    | IMPLY (p, q) -> not (eval p) || (eval q)
    | LESS (a, b) -> (calc a) < (calc b);;

(*
let bool_to_string e =
    match e with
    | true -> "true"
    | false -> "false";;

print_endline (bool_to_string (eval TRUE));;
print_endline (bool_to_string (eval (ORELSE (TRUE, FALSE))));;
print_endline (bool_to_string (eval (LESS (NUM 10, PLUS(NUM 5, NUM 4)))));;
*)
