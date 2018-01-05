type nat = ZERO
         | SUCC of nat

let rec natadd(a, b) = 
    match (a, b) with
    | (ZERO, _) -> b
    | (_, ZERO) -> a
    | (SUCC(c), SUCC(d)) -> natadd(SUCC(SUCC(c)), d)

let rec natmul(a, b) =
    match (a, b) with
    | (ZERO, _) -> ZERO
    | (_, ZERO) -> ZERO
    | (SUCC(c), SUCC(d)) ->
      natadd(natmul(c, d), natadd(natadd(c, d), SUCC(ZERO)))
