type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat =
 fun n1 n2 ->
  let rec s p q = match p with ZERO -> q | SUCC r -> s r (SUCC q) in
  s n1 n2


let rec natmul : nat -> nat -> nat =
 fun n1 n2 ->
  let rec mm p q =
    match p with SUCC ZERO -> q | SUCC r -> mm r (natadd q q)
  in
  if n1 = ZERO || n2 = ZERO then ZERO else mm n1 n2
