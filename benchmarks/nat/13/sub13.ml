type nat = ZERO | SUCC of nat
let rec natadd (n1, n2) =
    match n1 with
    | ZERO -> n2
    | SUCC s -> natadd (s, SUCC n2)
                        
let rec natmul (n1, n2) =
    match n1 with
    | ZERO -> ZERO
    | SUCC s -> natadd (n2, natmul(s, n2))
