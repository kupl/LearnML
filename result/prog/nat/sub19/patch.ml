type nat = ZERO | SUCC of nat

let natadd (n1 : nat) (n2 : nat) : nat =
  let rec compare (a : nat) (b : nat) (i : nat) : nat =
    if a = i then b else compare a (SUCC b) (SUCC i)
  in
  compare n1 n2 ZERO


let two : nat = SUCC (SUCC ZERO)

let three : nat = SUCC (SUCC (SUCC ZERO))

let (_ : nat) = natadd two three

let natmul (n1 : nat) (n2 : nat) : nat =
  if n1 = ZERO || n2 = ZERO then ZERO
  else
    let rec multi (a : nat) (b : nat) (i : nat) : nat =
      if a = i then b else natadd b (multi a b (SUCC i))
    in
    multi n1 n2 (SUCC ZERO)


let (_ : nat) = natmul two three
