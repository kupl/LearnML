
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

let rec calc : exp -> int = fun e ->
    match e with
    | Num i -> i
    | Plus (e1, e2) -> calc e1 + calc e2
    | Minus (e1, e2) -> calc e1 - calc e2

let rec eval : formula -> bool = fun f -> 
   match f with
   | True -> true
   | False -> false
   | Not f1 -> not (eval f1)
   | AndAlso (f1, f2) -> (eval f1) && (eval f2)
   | OrElse (f1, f2) -> (eval f1) || (eval f2)
   | Imply (f1, f2) -> if eval f1 == true && eval f2 == false then false
                       else true
   | Equal (e1, e2) -> if calc e1 = calc e2 then true
                      else false

(*

let _ = print_endline (string_of_bool (eval True))
let _ = print_endline (string_of_bool (eval False))
let _ = print_endline (string_of_bool (eval(Not True)))
let _ = print_endline (string_of_bool (eval(AndAlso(True, False))))
let _ = print_endline (string_of_bool (eval(OrElse(True, False))))
let _ = print_endline (string_of_bool (eval(Imply(True, False))))

let a = Num 10
let b = Num 15
let c = Plus(a, b)
let d = Minus(a, b)

let _ = print_endline (string_of_bool (eval(Equal(a, b))))
let _ = print_endline (string_of_bool (eval(Equal(c, b))))
let _ = print_endline (string_of_bool (eval(Equal(d, b))))

*)
