(* problem 2 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1, n2 with
|ZERO, ZERO -> ZERO
|ZERO, nat2 -> nat2
|nat1, ZERO -> nat1
|SUCC a, SUCC b ->SUCC(SUCC (natadd a b));;


let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->match n1, n2 with
|ZERO, ZERO -> ZERO
|ZERO, nat2 -> ZERO
|nat1, ZERO -> ZERO
|SUCC ZERO, nat2 ->nat2
|nat1, SUCC ZERO -> nat1
|nat1, SUCC b ->natadd (natmul (SUCC ZERO) nat1) (natmul nat1 b) ;;

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n1, n2 with
|ZERO, nat2-> ZERO
|SUCC ZERO, nat2-> SUCC ZERO
|nat1, ZERO -> SUCC ZERO
|nat1, SUCC b-> natmul nat1 (natexp nat1 b);;