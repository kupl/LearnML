type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat =
 fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC ZERO -> SUCC n2
  | SUCC x -> SUCC (natadd x n2)


let natmul : nat -> nat -> nat =
 fun n1 n2 ->
  match n1 with ZERO -> ZERO | SUCC ZERO -> n2 | SUCC nat -> natadd n2 n2


let two = SUCC (SUCC ZERO)

let three = SUCC (SUCC (SUCC ZERO))

let five = SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))

let _ = natadd two three

let _ = natadd three five

let _ = natmul two three

let _ = natmul two two
