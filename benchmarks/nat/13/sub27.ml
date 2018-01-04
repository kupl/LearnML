type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
        match (a, b) with
        | (ZERO, ZERO) -> ZERO
        | (h, ZERO) -> h
        | (ZERO, t) -> t
        | (h, SUCC n) -> natadd((SUCC h), n)

(* a * b = a + a * (b - 1) *)
let rec natmul (a, b) = 
        match (a, b) with
        | (_, ZERO) -> ZERO
        | (ZERO, _) -> ZERO
        | (h, SUCC n) -> natadd(h, natmul(h, n))