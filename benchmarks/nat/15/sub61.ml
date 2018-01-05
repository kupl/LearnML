type nat = ZERO | SUCC of nat

let rec natadd(x,y) =
    if y = ZERO then x
    else let SUCC(y) = y in
    SUCC(natadd(x,y))

let rec natmul(x,y) = 
    if y = ZERO then ZERO
    else let SUCC(y) = y in
    natadd(x,natmul(x,y))
