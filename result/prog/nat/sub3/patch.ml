type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  match n1 with ZERO -> n2 | SUCC n1' -> SUCC (natadd n1' n2)


let rec natmul (n1 : nat) (n2 : nat) : nat =
  match n1 with
  | ZERO -> ZERO
  | SUCC ZERO -> n2
  | _ -> (
      match n2 with
      | ZERO -> ZERO
      | SUCC __s12 ->
          let __s13 : nat = n2 in
          natadd n1 (natmul n1 __s12) )
