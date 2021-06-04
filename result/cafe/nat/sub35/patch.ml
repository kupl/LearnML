type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC x -> SUCC (natadd x n2)


let natmul (n1 : nat) (n2 : nat) : nat =
  let rec natadd (n1 : nat) (n2 : nat) : nat =
    match n1 with ZERO -> n2 | SUCC x -> SUCC (natadd x n2)
  in

  let rec mul (x : nat) (y : nat) : nat =
    match x with ZERO -> y | SUCC ZERO -> y | SUCC x' -> mul x' (natadd n2 y)
  in
  if n1 = ZERO || n2 = ZERO then ZERO else mul n1 n2


let two : nat = SUCC (SUCC ZERO)

let three : nat = SUCC (SUCC (SUCC ZERO))

let (_ : nat) = natadd two three

let (_ : nat) = natmul two three
