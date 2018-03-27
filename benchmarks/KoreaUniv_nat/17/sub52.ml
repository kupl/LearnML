(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC (remains) -> natadd remains (SUCC (n2))


let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match (n1, n2) with
  | (ZERO, _) -> ZERO
  | (_, ZERO) -> ZERO
  | ((SUCC ZERO), n) -> n
  | (n, (SUCC ZERO)) -> n
  | ((SUCC (remains)), n) -> natadd n (natmul remains n)


let rec natexp : nat -> nat -> nat
= fun n1 n2 ->
  match (n1, n2) with
  | (_, ZERO) -> (SUCC ZERO)
  | (ZERO, _) -> ZERO
  | ((SUCC ZERO), n) -> (SUCC ZERO)
  | (n, (SUCC ZERO)) -> n
  | (n, (SUCC (remains))) -> natmul n (natexp n remains)
