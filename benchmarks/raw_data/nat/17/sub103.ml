  (* problem 2*)
  type nat =
   ZERO
  | SUCC of nat

  let rec natadd : nat -> nat -> nat 
  = fun n1 n2 -> (* TODO *)
  match n2 with
  |ZERO->n1
  |SUCC k2-> (SUCC (natadd n1 k2))
  let rec natmul : nat -> nat -> nat 
  = fun n1 n2 -> (* TODO *)
      match n2 with
      |ZERO->n1
      |SUCC k -> if k=ZERO then n1 else natadd n1 (natmul n1 k) 
  
  let rec natexp : nat -> nat -> nat 
  = fun n1 n2 -> (* TODO *)
   match n2 with
   |ZERO->SUCC ZERO
   |SUCC k -> if k=ZERO then SUCC ZERO else natmul (natmul n1 k) n1
