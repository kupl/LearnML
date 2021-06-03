type formula = True
            | False
            | Not of formula
            | AndAlso of formula * formula
            | OrElse of formula * formula
            | Imply of formula * formula
            | Equal of exp * exp
and exp = Num of int
        | Plus of exp * exp
        | Minus of exp * exp

let rec calc e =
    match e with
    | Num (n) -> n
    | Plus (n, m) -> calc(n) + calc(m)
    | Minus (n, m) -> calc(n) - calc(m);;

let rec eval f =
    match f with
    | True -> true
    | False -> false
    | Not p -> not (eval p)
    | AndAlso (p, q) -> (eval p) && (eval q)
    | OrElse (p, q) -> (eval p) || (eval q)
    | Imply (p, q) -> not (eval p) || (eval q)
    | Equal (a, b) -> (calc a) = (calc b);;

(*
let bool_to_string e =
    match e with
    | true -> "true"
    | false -> "false";;

print_endline (bool_to_string (eval True));;
print_endline (bool_to_string (eval (OrElse (True, False))));;
print_endline (bool_to_string (eval (Equal (Num 10, Plus(Num 5, Num 4)))));;
*)
