type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
;;

type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
;;

let rec calc e =
        match e with
        NUM a -> a
        |PLUS(a,b) -> calc a+ calc b
        |MINUS(a,b) -> calc a- calc b
;;

let rec eval f =
        match f with
        TRUE -> true
        |FALSE -> false
        |NOT a -> if eval a then false else true
        |ANDALSO(a,b) -> eval a && eval b
        |ORELSE(a,b) -> eval a || eval b
        |IMPLY(a,b) -> if eval a = true && eval b = false then false else true
        |LESS(c,d) -> calc c < calc d
;;
