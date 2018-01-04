type nat=ZERO
        |SUCC of nat

let rec natadd (a,b):nat=
        match b with
        |ZERO->a
        |SUCC k->(natadd ((SUCC a), k))

let rec natmul (a,b):nat=
        match b with
        |ZERO->ZERO
        |SUCC k->(natadd (a ,(natmul (a,k))))





