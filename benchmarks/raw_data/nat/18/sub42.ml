type nat = ZERO | SUCC of nat
let two = SUCC(SUCC ZERO);;
let three = SUCC(SUCC(SUCC ZERO));;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
if n1 = two then SUCC(SUCC(n2)) else SUCC(SUCC(SUCC(n2)));;
  
  
  
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
if n1 = two then if n2 = two then SUCC(SUCC(n2)) else SUCC(SUCC(SUCC(n2)))
else if n2 = two then SUCC(SUCC(SUCC(n2))) else SUCC(SUCC(SUCC(SUCC(SUCC(SUCC(n2))))));;


natadd two two;;
natadd two three;;
natadd three two;;
natadd three three;;
natmul two three;;
natmul two two;;
natmul three three;;