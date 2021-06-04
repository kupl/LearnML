type nat = ZERO | SUCC of nat

let rec natadd (n1 : nat) (n2 : nat) : nat =
  let rec s (p : nat) (q : nat) : nat =
    match p with ZERO -> q | SUCC r -> s r (SUCC q)
  in
  s n1 n2


let rec natmul (n1 : nat) (n2 : nat) : nat =
  let rec mm (p : nat) (q : nat) : nat =
    match p with SUCC ZERO -> q | SUCC r -> mm r (natadd q q)
  in
  if n1 = ZERO || n2 = ZERO then ZERO else mm n1 n2
