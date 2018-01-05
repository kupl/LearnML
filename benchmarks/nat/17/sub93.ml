type nat = ZERO| SUCC of nat

let rec natadd (a, b) =
    match a with
    |ZERO -> b
    |SUCC a' -> match b with
                |ZERO -> a
                |SUCC b' -> SUCC(natadd(a', b))

let rec natmul (a, b) = 
    match a with
    |ZERO -> ZERO
    |SUCC a' -> match b with
                |ZERO -> ZERO
                |SUCC b' -> natadd(a, natmul(a, b')) 
