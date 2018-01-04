type nat = ZERO
         | SUCC of nat

let rec count nat =
  match nat with
  | ZERO -> 0
  | SUCC a -> 1 + (count a)

let rec construct num =
  if num = 0 then ZERO
  else SUCC (construct (num - 1))

let natadd expr =
  construct ((count (fst expr)) + (count (snd expr)))

let natmul expr =
  construct ((count (fst expr)) * (count (snd expr)))
