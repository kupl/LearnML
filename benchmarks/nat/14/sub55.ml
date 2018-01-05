type nat = ZERO | SUCC of nat

let rec natadd (a,b) =
    match a with
        |ZERO -> b
        |SUCC tmp -> natadd (tmp,SUCC(b))

let rec natmul (a,b) =
    match a with
        |ZERO -> ZERO
        |SUCC tmp -> natadd (natmul(tmp,b),b)

