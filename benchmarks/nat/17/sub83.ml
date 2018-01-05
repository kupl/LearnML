type nat = ZERO | SUCC of nat

let rec natadd: nat * nat -> nat = fun (a, b) ->
  match a with
    | ZERO -> b
    | SUCC(_a) -> natadd(_a, SUCC(b))

let rec natmul: nat * nat -> nat = fun (a, b) ->
  match a with
    | ZERO -> ZERO
    | SUCC(_a) -> natadd (natmul (_a, b), b)
