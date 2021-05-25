(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
  ZERO -> (match n2 with
    ZERO -> ZERO
  | SUCC m2 -> SUCC (natadd n1 m2) )
| SUCC m1 -> (match n2 with
    ZERO -> SUCC (natadd m1 n2)
  | SUCC m2 -> SUCC (SUCC (natadd m1 m2) ) )

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
  ZERO -> ZERO 
| _ -> (match n2 with
    ZERO -> ZERO
  | SUCC m2 -> (natadd n1 (natmul n1 m2)))

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
  ZERO -> (SUCC ZERO)
| _ -> (match n2 with
    ZERO -> (SUCC ZERO)
    | SUCC m2 -> (natmul n1 (natexp n1 m2)))
