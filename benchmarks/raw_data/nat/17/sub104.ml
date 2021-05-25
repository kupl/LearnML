(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
  match n2 with    (* check the n2 first  *)
  | ZERO ->         (*  if n2 is ZERO, evaluate the value of n1 *)
    let rec evaln1 n =      (* evaluation function for n1  *)  
      match n with 
      | ZERO -> ZERO
      | SUCC n1_1 -> SUCC (evaln1 n1_1) (*  by tail recursive form, it evaluate n1 *)
    in evaln1 n1    (* evaluate n1 *)
  | SUCC n2_1 -> SUCC (natadd n1 n2_1)  
(* if n2 is not ZERO, tail recursively call natadd function by, 'evaln1', the evaluated value of n1 is accumulated. *)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
  match n2 with
  | ZERO -> ZERO
  | SUCC n2_1 -> natadd n1 (natmul n1 n2_1) (* simply recall n2_1 times of (natadd n1)
 *)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> 
  match n2 with
  | ZERO -> SUCC ZERO  (* any number's exp is one  *)
  | SUCC n2_1 -> natmul n1 (natexp n1 n2_1) (* call natmul by n2 times with n1 value*)