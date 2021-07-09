type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC n3 -> natadd n3 (SUCC n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n1 with
  | ZERO -> ZERO
  | SUCC ZERO -> n2
  | SUCC n3 -> natmul n3 (natadd n2 n2)


let two : nat = SUCC (SUCC ZERO)

let three : nat = SUCC (SUCC (SUCC ZERO))

let (_ : nat) = natmul two three

let (_ : nat) = natadd two three
