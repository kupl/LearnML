type nat =
    | ZERO
    | SUCC of nat;;

let rec natadd (n1,n2) =
    match (n1,n2) with
    | (ZERO,ZERO) -> ZERO
    | (SUCC i,x) | (x,SUCC i) -> SUCC (natadd (i,x))

let rec natmul (n1,n2) =
    match (n1,n2) with
    | (ZERO,ZERO) -> ZERO
    | (SUCC i,x) | (x,SUCC i) -> natadd (x,natmul (i,x))
