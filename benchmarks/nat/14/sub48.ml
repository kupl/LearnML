type nat = ZERO | SUCC of nat

let rec natadd: nat * nat -> nat =
    fun (a, b) ->
        match (a, b) with
        | (ZERO, x) -> x
        | (x, ZERO) -> x
        | (x, SUCC y) -> (natadd ((SUCC x), y))

let rec iter_mul: nat * nat -> nat -> nat =
    fun (a, b) res ->
        match (a, b) with
        | (x, ZERO) -> res
        | (x, SUCC y) -> iter_mul (x, y) (natadd (res, x))

let rec natmul: nat * nat -> nat =
    fun (a, b) ->
        (iter_mul (a, b) ZERO)
