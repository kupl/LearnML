type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat =
 fun n1 n2 ->
  let rec compare a b i = if a = i then b else compare a (SUCC b) (SUCC i) in
  compare n1 n2 ZERO


let two = SUCC (SUCC ZERO)

let three = SUCC (SUCC (SUCC ZERO))

let _ = natadd two three

let natmul : nat -> nat -> nat =
 fun n1 n2 ->
  if n1 = ZERO || n2 = ZERO then ZERO
  else
    let rec multi a b i = if a = i then b else multi a (natadd n2 b) (SUCC i) in
    multi n1 n2 (SUCC ZERO)


let _ = natmul two three
