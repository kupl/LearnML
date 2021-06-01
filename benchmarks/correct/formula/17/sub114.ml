type exp = Num of int
    | Plus of exp * exp
    | Minus of exp * exp

type formula = True
    | False
    | Not of formula
    | AndAlso of formula * formula
    | OrElse of formula * formula
    | Imply of formula * formula
    | Equal of exp * exp

let rec getnum e : int = 
    match e with
    | Num a -> a
    | Plus (a, b) -> (getnum a) + (getnum b)
    | Minus (a, b) -> (getnum a) - (getnum b)

let rec eval f : bool =
    match f with
    | True -> true
    | False -> false
    | Not a -> not (eval a)
    | AndAlso (a, b) -> (eval a) && (eval b)
    | OrElse (a, b) -> (eval a) || (eval b)
    | Imply (a,b) -> if((eval a = true)&& (eval b) = false) then false else true
    | Equal (a, b) -> (getnum a) = (getnum b)

(*let a61 = eval True
let a62 = eval False
let a63 = eval (Not True)
let a64 = eval (AndAlso (True, False))
let a65 = eval (OrElse (True, False))
let a66 = eval (Equal (Plus(Num 3, Num 4), Minus(Num 7, Num 8))) 

let _ = print_endline(string_of_bool a61)
let _ = print_endline(string_of_bool a62)
let _ = print_endline(string_of_bool a63)
let _ = print_endline(string_of_bool a64)
let _ = print_endline(string_of_bool a65)
let _ = print_endline(string_of_bool a66)*)
