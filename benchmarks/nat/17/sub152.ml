
type nat = ZERO | SUCC of nat

let natadd : nat * nat -> nat = fun (x, y) ->
  let rec dedc z =
    match z with
      | ZERO -> y
      | SUCC z_ -> SUCC (dedc z_)
  in
    dedc x

let rec natmul : nat * nat -> nat = fun (x, y) ->
  let addy z = natadd (z, y) in
  let rec dedc z =
    match z with
      | ZERO -> ZERO
      | SUCC z_ -> addy (dedc z_)
  in
    dedc x
