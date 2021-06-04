type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat =
 fun n1 n2 -> match n1 with ZERO -> n2 | SUCC nest -> SUCC (natadd nest n2)


let rec natmul : nat -> nat -> nat =
 fun n1 n2 ->
  match (n1, n2) with
  | _, ZERO -> SUCC ZERO
  | ZERO, SUCC nest2 -> SUCC (natmul n1 nest2)
  | SUCC nest1, _ -> SUCC (natmul nest1 n2)
