type nat = ZERO | SUCC of nat

let rec __s24 (__s25 : nat) (__s26 : nat) : nat =
  match __s26 with ZERO -> __s25 | SUCC __s28 -> __s24 (SUCC __s25) __s28


let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC n1_next -> __s24 n2 n1


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n2 with ZERO -> ZERO | SUCC n2_next -> natadd n1 (natmul n1 n2_next)
