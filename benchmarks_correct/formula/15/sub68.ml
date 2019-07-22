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

(*
type expr_wrapper = POS of expr
        | NEG of expr

let calc e =
    let rec aux (e, next, result) = 
        match e with
        | POS(x) -> (
            match x with
            | NUM (n) -> (
                match next with
                | [] -> n + result
                | e::l -> aux(e, l, n + result))
            | PLUS (n, m) -> aux( POS(n), POS(m)::next, result )
            | MINUS (n, m) -> aux( POS(n), NEG(m)::next, result ))
        | NEG(n) -> - aux(POS(n), next, result)
    in aux (POS(e), [], 0);;
*)

let rec calc e = 
    match e with
    | NUM(n) -> n
    | PLUS(n, m) -> calc(n) + calc(m)
    | MINUS(n, m) -> calc(n) - calc(m)

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
