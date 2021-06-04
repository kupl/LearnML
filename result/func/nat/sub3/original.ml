type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC n1' -> SUCC (natadd n1' n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n1 with
  | ZERO -> ZERO
  | SUCC ZERO -> n2
  | SUCC n1' ->
      SUCC
        ( match n2 with
        | ZERO -> ZERO
        | SUCC ZERO -> SUCC ZERO
        | SUCC n2' -> SUCC (natmul n1' (natmul n1 n2')) )
