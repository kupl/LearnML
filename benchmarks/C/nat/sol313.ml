(* Try to write any programs in OCaml *)
type nat = ZERO | SUCC of nat;;


let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  ZERO -> n2
  |SUCC n1' -> natadd n1' (SUCC n2);;


let rec realNatmul
= fun n1 n2 n1_original -> match n2 with
  |ZERO -> ZERO
  |SUCC ZERO -> n1
  |SUCC n2' -> realNatmul (natadd n1 n1_original) n2' n1_original;; 

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> realNatmul n1 n2 n1;; 

