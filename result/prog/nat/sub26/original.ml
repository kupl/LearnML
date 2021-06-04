type nat = ZERO | SUCC of nat

let natdec (n : nat) : nat = match n with SUCC n' -> n' | ZERO -> ZERO

let rec natadd (n1 : nat) (n2 : nat) : nat =
  if n1 = ZERO then n2 else natadd (natdec n1) (SUCC n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  if n1 = SUCC ZERO then n2 else natadd n2 (natmul (natdec n1) n2)


let two : nat = SUCC (SUCC ZERO)

let three : nat = SUCC (SUCC (SUCC ZERO))

let (_ : nat) = natadd two three

let (_ : nat) = natmul two three
