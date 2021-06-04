type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with
  | ZERO -> n2
  | SUCC ZERO -> SUCC n2
  | SUCC x -> SUCC (natadd x n2)


let natmul (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> ZERO | SUCC ZERO -> n2 | SUCC nat -> natadd n2 n2


let two : nat = SUCC (SUCC ZERO)

let three : nat = SUCC (SUCC (SUCC ZERO))

let five : nat = SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))

let (_ : nat) = natadd two three

let (_ : nat) = natadd three five

let (_ : nat) = natmul two three

let (_ : nat) = natmul two two
