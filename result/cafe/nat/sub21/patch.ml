type nat = ZERO | SUCC of nat

let two : nat = SUCC (SUCC ZERO)

let three : nat = SUCC (SUCC (SUCC ZERO))

let rec natadd (n1 : nat) (n2 : nat) : nat =
  if n1 = two then SUCC (SUCC n2) else SUCC (SUCC (SUCC n2))


let rec __s4 (__s5 : nat) (__s6 : nat) (__s7 : nat) : nat =
  match __s5 with
  | ZERO -> ZERO
  | SUCC __s21 -> (
      match __s6 with
      | ZERO -> __s4 __s21 __s7 __s7
      | SUCC __s22 -> SUCC (__s4 __s5 __s22 __s7) )


let rec natmul (n1 : nat) (n2 : nat) : nat = __s4 n2 n1 n1

let (_ : nat) = natadd two two

let (_ : nat) = natadd two three

let (_ : nat) = natadd three two

let (_ : nat) = natadd three three

let (_ : nat) = natmul two three

let (_ : nat) = natmul two two

let (_ : nat) = natmul three three
