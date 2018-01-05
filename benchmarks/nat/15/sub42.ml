type nat =
    ZERO
    | SUCC of nat

let rec natadd (a,b) =
    match a with
    | ZERO -> b
    | SUCC arg -> natadd (arg,SUCC(b))

and natmul (a,b) =
    match (a,b) with
    | (ZERO,arg) -> ZERO
    | (arg,ZERO) -> ZERO
    | (SUCC(arg1),arg2) -> natadd(natmul(arg1,arg2),arg2)


