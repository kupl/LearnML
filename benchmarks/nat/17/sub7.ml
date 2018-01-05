type nat = ZERO | SUCC of nat

let rec natadd = (fun (x, y) ->
  match (x, y) with
  | (ZERO, _) -> y
  | (_, ZERO) -> x
  | (SUCC a, SUCC b) -> natadd (SUCC x, b)
)

let rec natmul = (fun (x, y) ->
  match (x, y) with
  | (ZERO, _) -> ZERO
  | (_, ZERO) -> ZERO
  | (SUCC ZERO, _) -> y
  | (_, SUCC ZERO) -> x
  | (a, SUCC b) -> natadd(a, natmul (a, b))
)
