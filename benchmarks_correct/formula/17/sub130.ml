(* exercise 1*)
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

let rec eval evalin =
        let rec num numin =
               match numin with
               | NUM(n) -> n
               | PLUS(n, m) -> ((num n) + (num m))
               | MINUS(n, m) -> ((num n) - (num m))
        in
        match evalin with
        | TRUE -> true
        | FALSE -> false
        | NOT(x) -> (not (eval x))
        | ANDALSO(x, y) -> ((eval x) && (eval y))
        | ORELSE(x, y) -> ((eval x) || (eval y))
        | IMPLY(x, y) -> ((not (eval x)) || (eval y))
        | LESS(w, z) -> ((num w) < (num z))
