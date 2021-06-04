type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat =
 fun n1 n2 -> match n1 with ZERO -> n2 | SUCC x -> SUCC (natadd x n2)


let natmul : nat -> nat -> nat =
 fun n1 n2 ->
  let rec natadd : nat -> nat -> nat =
   fun n1 n2 -> match n1 with ZERO -> n2 | SUCC x -> SUCC (natadd x n2)
  in

  let rec mul x y = match x with ZERO -> y | SUCC x' -> mul x' (natadd x y) in
  if n1 = ZERO || n2 = ZERO then ZERO else mul n1 n2


let two = SUCC (SUCC ZERO)

let three = SUCC (SUCC (SUCC ZERO))

let _ = natadd two three

let _ = natmul two three
