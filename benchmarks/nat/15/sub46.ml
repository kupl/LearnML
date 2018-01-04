type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (x, y) ->
  match (x, y) with
  | (ZERO, y) -> y
  | (x, ZERO) -> x
  | (SUCC a, SUCC b) -> (SUCC (SUCC (natadd (a, b))))

let rec natmul : nat * nat -> nat = fun (x, y) ->
  match (x, y) with
  | (ZERO, y) -> ZERO
  | (x, ZERO) -> ZERO
  | (_, SUCC b) -> natadd (x, natmul (x, b))
