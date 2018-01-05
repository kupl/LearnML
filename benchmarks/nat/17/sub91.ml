type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (a, b) ->
 match (a, b) with
 | ZERO, _ -> b
 | _, ZERO -> a
 | SUCC i, SUCC j -> SUCC (SUCC (natadd (i, j)))

let rec natmul : nat * nat -> nat = fun (a, b) ->
  match b with
  | ZERO -> ZERO
  | SUCC i -> natadd(a, (natmul(a, i)))
