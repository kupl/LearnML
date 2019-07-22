(* 2015 - 14718 Giyeon Kim HW 2 *)

(* Exercise 1 *)
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

let rec calc: expr -> int = fun expr ->
    match expr with
    | NUM i -> i
    | PLUS (l, r) -> calc l + calc r
    | MINUS (l, r) -> calc l - calc r

let rec eval: formula -> bool = fun form ->
    match form with
    | TRUE -> true
    | FALSE -> false
    | NOT f -> not (eval f)
    | ANDALSO (l, r) -> (eval l) && (eval r)
    | ORELSE (l, r) -> (eval l) || (eval r)
    | IMPLY (l, r) -> not (eval l) || (eval r)
    | LESS (lexpr, rexpr) -> (calc lexpr) < (calc rexpr)


