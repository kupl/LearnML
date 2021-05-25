(* 2015 - 14718 Giyeon Kim HW 2 *)

(* Exercise 1 *)
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

let rec calc: exp -> int = fun exp ->
    match exp with
    | Num i -> i
    | Plus (l, r) -> calc l + calc r
    | Minus (l, r) -> calc l - calc r

let rec eval: formula -> bool = fun form ->
    match form with
    | True -> true
    | False -> false
    | Not f -> not (eval f)
    | AndAlso (l, r) -> (eval l) && (eval r)
    | OrElse (l, r) -> (eval l) || (eval r)
    | Imply (l, r) -> not (eval l) || (eval r)
    | Equal (lexp, rexp) -> (calc lexp) = (calc rexp)


