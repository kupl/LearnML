type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC nest -> SUCC (natadd nest n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n2 with
  | SUCC __s7 ->
      let __s8 : nat = natmul __s7 n1 in
      natadd __s8 n1
  | ZERO -> ZERO
