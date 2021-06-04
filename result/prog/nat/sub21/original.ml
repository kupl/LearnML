type nat = ZERO | SUCC of nat

let two : nat = SUCC (SUCC ZERO)

let three : nat = SUCC (SUCC (SUCC ZERO))

let rec natadd (n1 : nat) (n2 : nat) : nat =
  if n1 = two then SUCC (SUCC n2) else SUCC (SUCC (SUCC n2))


let rec natmul (n1 : nat) (n2 : nat) : nat =
  if n1 = two then if n2 = two then SUCC (SUCC n2) else SUCC (SUCC (SUCC n2))
  else if n2 = two then SUCC (SUCC (SUCC n2))
  else SUCC (SUCC (SUCC (SUCC (SUCC (SUCC n2)))))


let (_ : nat) = natadd two two

let (_ : nat) = natadd two three

let (_ : nat) = natadd three two

let (_ : nat) = natadd three three

let (_ : nat) = natmul two three

let (_ : nat) = natmul two two

let (_ : nat) = natmul three three
