(*컴퓨터공학부/2011-11729/안진우/2-1*)

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

let rec calc x : int =
       match x with       
       | Num (y) -> y
       | Plus (y, z) -> calc(y) + calc(z)
       | Minus (y, z) -> calc(y) - calc(z)


let rec eval x : bool =
        match x with
        | True -> true
        | False -> false
        | Not y -> not(eval(y))
        | AndAlso (y, z) -> eval(y) && eval(z)
        | OrElse (y, z) -> eval(y) || eval(z)
        | Imply (y, z) -> (
                        if eval(y) == false then true
                        else if eval(z) == true then true
                        else false
                        )
        | Equal (y, z) -> calc(y) = calc(z)

