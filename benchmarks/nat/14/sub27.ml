exception TODO

type nat =
    | ZERO
    | SUCC of nat

let rec natadd ((p:nat), (q:nat)): nat =
    match (p, q) with
    | (ZERO, _ ) -> q
    | (_, ZERO) -> p
    | ((SUCC m), _) -> natadd (m, (SUCC q))

let rec natmul ((p:nat), (q:nat)): nat =
    match (p, q) with
    | (ZERO, _) -> ZERO
    | (_, ZERO) -> ZERO
    | ((SUCC ZERO), r) -> r
    | (r, (SUCC ZERO)) -> r
    | ((SUCC m), n) -> natadd (n, natmul (m, n))
