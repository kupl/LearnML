type nat = ZERO | SUCC of nat


let rec natadd (nat1, nat2) =
        match (nat1, nat2) with
        | (ZERO, SUCC n1) -> SUCC n1
        | (SUCC n1, ZERO) -> SUCC n1
        | (SUCC n1, SUCC n2) -> natadd(SUCC(SUCC n1), n2)
        | (ZERO, ZERO) -> ZERO

let rec natmul (nat1, nat2) = 
        match (nat1, nat2) with
        | (ZERO, SUCC n1) -> ZERO
        | (SUCC n1, ZERO) -> ZERO
        | (ZERO, ZERO) -> ZERO
        | (SUCC n1, n2) -> natadd(n2, natmul(n1, n2))



