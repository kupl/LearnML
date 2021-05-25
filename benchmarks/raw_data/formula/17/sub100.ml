type formula = True
             | False
             | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp
and  exp = Num of int
          | Plus of exp * exp
          | Minus of exp * exp

let rec eval formula =
    let rec calc exp =
        match exp with
        | Num(n) -> n
        | Plus(exp1, exp2) -> (calc exp1) + (calc exp2)
        | Minus(exp1, exp2) -> (calc exp1) - (calc exp2)
    in
    match formula with
    | True -> true
    | False -> false
    | Not(fm) -> not (eval fm)
    | AndAlso(fm1, fm2) -> (eval fm1) && (eval fm2)
    | OrElse(fm1, fm2) -> (eval fm1) || (eval fm2)
    | Imply(fm1, fm2) -> (not (eval fm1)) || (eval fm2)
    | Equal(exp1, exp2) -> (calc exp1) = (calc exp2)
