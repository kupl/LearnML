(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> let rec f n1 n2 = match n1 with
               | SUCC(m1) -> f m1 (SUCC(n2))
               | ZERO -> n2 in
               f n1 n2

let natmul : nat -> nat -> nat 
= fun n1 n2 -> let rec f n1 n2 = 
               match n1 with
               | ZERO -> ZERO 
               | SUCC(m) -> natadd n2 (f m n2) in
               f n1 n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> let rec f n1 n2 = 
               match n2 with
               | ZERO -> SUCC(ZERO) 
               | SUCC(m) -> natmul n1 (f n1 m) in
               f n1 n2
