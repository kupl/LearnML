

(*
    컴퓨터공학부 2012-11270 장선웅
    hw 1 - Exercise 4

*)

type nat = ZERO | SUCC of nat

let rec natadd  = fun (a,b) ->
    match a with
    |ZERO -> b
    |SUCC(a') -> SUCC( natadd(a',b))

let rec natmul  = fun (a,b) ->
    match b with
    | ZERO -> ZERO
    | SUCC(b') -> 
        ( match b' with
        | ZERO -> a
        | SUCC(b'') -> natadd(a,natmul(a,b'))
        )





