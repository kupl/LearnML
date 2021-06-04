type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC nest -> SUCC (natadd nest n2)


let rec __s4 (__s5 : nat) (__s6 : nat) (__s7 : nat) : nat =
  match __s5 with
  | ZERO -> ZERO
  | SUCC __s21 -> (
      match __s6 with
      | ZERO -> __s4 __s21 __s7 __s7
      | SUCC __s22 -> SUCC (__s4 __s5 __s22 __s7) )


let rec natmul (n1 : nat) (n2 : nat) : nat = __s4 n1 n2 n2
