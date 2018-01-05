type nat = ZERO | SUCC of nat

let rec natadd (a,b) = 
    match b with
    | ZERO -> a
    | SUCC(c) -> SUCC (natadd(a, c))
 
let rec natmul (a,b) = 
    match b with
    | ZERO -> ZERO
    | SUCC (c) -> natadd (a, natmul(a,c))