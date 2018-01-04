type nat = ZERO
         | SUCC of nat

let rec natadd: nat * nat -> nat =
  fun (n1, n2) ->
    match (n1, n2) with
    | (_, ZERO) ->
        n1
    | (ZERO, _) ->
        n2
    | (_, SUCC n) -> natadd (SUCC n1, n)

let rec natmul: nat * nat -> nat =
  fun (n1, n2) ->
    match (n1, n2) with
    | (_, ZERO) -> ZERO
    | (ZERO, _) -> ZERO
    | (_, SUCC n) -> natadd (n1, natmul (n1, n))
