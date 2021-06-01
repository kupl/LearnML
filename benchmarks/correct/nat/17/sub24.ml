(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC (nat) -> SUCC (natadd nat n2)
;;

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | SUCC (nat) -> natadd (natmul nat n2) n2
;;

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  | ZERO -> SUCC ZERO (* A lot of people argue that 0^0=1 *)
  | SUCC (nat2) -> (
    match n1 with
    | ZERO -> ZERO
    | SUCC (nat1) -> natmul n1 (natexp n1 nat2)
  )
;;
