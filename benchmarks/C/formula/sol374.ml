type formula =
        | True
        | False
        | Not of formula
        | AndAlso of formula * formula
        | OrElse of formula * formula
        | Imply of formula * formula
        | Equal of exp * exp

and exp =
        | Num of int
        | Plus of exp * exp
        | Minus of exp * exp
let rec cnt ex =
match ex with
        |Num(a) ->a
        |Plus(a,b)->cnt(a)+cnt(b)
        |Minus(a,b) -> cnt(a)-cnt(b)


let rec eval : formula -> bool
= fun f ->
match f with
|True -> true
|False -> false
|Not(formula) ->
        if eval(formula)=true then false
        else true
|AndAlso(a, b) ->
        if eval(a)=true && eval(b)= true then true
        else false
|OrElse(a,b) ->
        if eval(a) =true || eval(b) = true then true
        else false
|Imply(a,b) ->
        if eval(a)=false || eval(b) = true then true
        else false
|Equal(a,b)->
        if cnt(a)=cnt(b) then true
        else false