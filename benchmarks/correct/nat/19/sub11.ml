type nat = ZERO | SUCC of nat ;; 
  
  
  let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match n1 with
  | ZERO -> n2
  | SUCC(w) -> natadd w (SUCC(n2))
  ;;
  

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match n1 with 
  | ZERO -> ZERO
  | SUCC(w) -> natadd n2 (natmul w n2)
  ;;
  