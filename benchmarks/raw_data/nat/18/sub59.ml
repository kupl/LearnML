type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  match n2 with
    | ZERO -> n1
    | SUCC n2_peel -> SUCC(natadd n1 n2_peel);;
  (*
  let rec nat2int mynat = 
    match mynat with
      | ZERO -> 0
      | SUCC mynat2 -> 1 + nat2int mynat2
  in
  let rec addnat mynat n = 
    if n=0 then mynat
    else SUCC(addnat mynat (n-1))
  in addnat n2 (nat2int n1);;
  *)
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  match n1 with
    | ZERO -> ZERO
    | SUCC n1_peel -> natadd (natmul n1_peel n2) n2;;
    
    
let two = SUCC (SUCC ZERO);;
let three = SUCC(SUCC(SUCC ZERO));;
natadd two three;;
natmul two three;;
natmul ZERO two;;
natmul two ZERO;;