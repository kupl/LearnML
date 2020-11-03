(* exercise 1*)
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

let rec eval evalin =
        let rec num numin =
               match numin with
               | Num(n) -> n
               | Plus(n, m) -> ((num n) + (num m))
               | Minus(n, m) -> ((num n) - (num m))
        in
        match evalin with
        | True -> true
        | False -> false
        | Not(x) -> (not (eval x))
        | AndAlso(x, y) -> ((eval x) && (eval y))
        | OrElse(x, y) -> ((eval x) || (eval y))
        | Imply(x, y) -> ((not (eval x)) || (eval y))
        | Equal(w, z) -> ((num w) = (num z))
