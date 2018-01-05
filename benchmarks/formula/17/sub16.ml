(*컴퓨터공학부/2011-11729/안진우/2-1*)

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

let rec calc x : int =
       match x with       
       | NUM (y) -> y
       | PLUS (y, z) -> calc(y) + calc(z)
       | MINUS (y, z) -> calc(y) - calc(z)


let rec eval x : bool =
        match x with
        | TRUE -> true
        | FALSE -> false
        | NOT y -> not(eval(y))
        | ANDALSO (y, z) -> eval(y) && eval(z)
        | ORELSE (y, z) -> eval(y) || eval(z)
        | IMPLY (y, z) -> (
                        if eval(y) == false then true
                        else if eval(z) == true then true
                        else false
                        )
        | LESS (y, z) -> calc(y) < calc(z)

