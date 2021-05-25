

(* problem 2*)
type nat = ZERO | SUCC of nat
let two = SUCC (SUCC ZERO)
 let three = SUCC (SUCC (SUCC ZERO))
 let rec natadd : nat -> nat -> nat 
     =fun n1 n2 ->
      match n1 with
      | ZERO -> n2
      | SUCC n1 -> SUCC (natadd n1 n2)
