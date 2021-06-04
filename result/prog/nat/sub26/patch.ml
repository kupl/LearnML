type nat = ZERO | SUCC of nat

let natdec (n : nat) : nat = match n with SUCC n' -> n' | ZERO -> ZERO

let rec natadd (n1 : nat) (n2 : nat) : nat =
  if n1 = ZERO then n2 else natadd (natdec n1) (SUCC n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n2 with
  | ZERO -> ZERO
  | SUCC ZERO -> n1
  | SUCC (SUCC ZERO) -> natmul n2 n1
  | _ -> natadd n2 (natmul n2 (natdec n1))


let two : nat = SUCC (SUCC ZERO)

let three : nat = SUCC (SUCC (SUCC ZERO))

let (_ : nat) = natadd two three

let (_ : nat) = natmul two three
