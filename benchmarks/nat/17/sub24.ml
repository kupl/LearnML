type nat = ZERO | SUCC of nat

let rec natadd (x, y) =
        match (x, y) with
        |(ZERO, _) -> y
        |(SUCC z, _) -> SUCC (natadd (z, y))

let rec natmul_h (x, y, carry) = 
        match (x, y) with
        |(ZERO, _) -> ZERO
        |(_, ZERO) -> ZERO
        |(SUCC ZERO, _) -> y
        |(SUCC z, _) -> natmul_h (z, natadd(carry, y), carry)

let natmul (x, y) =
        natmul_h (x, y, y)
