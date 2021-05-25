(* problem 2 *)
type nat = ZERO | SUCC of nat

let natadd : nat->nat->nat
= fun n1 n2 -> let rec f n1 n2 =
  match n2 with
   ZERO -> n1
  |SUCC n -> f (SUCC n1) n
  in f n1 n2

let natmul : nat->nat->nat
= fun n1 n2 -> let rec f n1 n2 =
match n2 with
 ZERO -> ZERO 
 |SUCC n -> natadd n1 (f n1 n)
 in f n1 n2

let natexp : nat->nat->nat
= fun n1 n2 -> let rec f n1 n2 = 
match n2 with
 ZERO -> SUCC ZERO
|SUCC n -> natmul n1 (f n1 n)
  in f n1 n2
