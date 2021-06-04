type nat = ZERO | SUCC of nat

let two = SUCC (SUCC ZERO)

let three = SUCC (SUCC (SUCC ZERO))

let rec natadd : nat -> nat -> nat =
 fun n1 n2 -> if n1 = two then SUCC (SUCC n2) else SUCC (SUCC (SUCC n2))


let rec natmul : nat -> nat -> nat =
 fun n1 n2 ->
  if n1 = two then if n2 = two then SUCC (SUCC n2) else SUCC (SUCC (SUCC n2))
  else if n2 = two then SUCC (SUCC (SUCC n2))
  else SUCC (SUCC (SUCC (SUCC (SUCC (SUCC n2)))))


let _ = natadd two two

let _ = natadd two three

let _ = natadd three two

let _ = natadd three three

let _ = natmul two three

let _ = natmul two two

let _ = natmul three three
