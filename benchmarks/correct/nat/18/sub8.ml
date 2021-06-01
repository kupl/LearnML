(* type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> *)

(*
let natmul : nat -> nat -> nat
= fun n1 n2 -> *)
type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
    ZERO -> n2
  | SUCC n1' -> natadd n1' (SUCC n2);;
  (* n1이 제로가 아닐때 *)
  
let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
    ZERO -> ZERO
  | SUCC n1' -> natadd (natmul n1' n2) n2;;



(* Test case
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

natmul two two;;

  *)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  