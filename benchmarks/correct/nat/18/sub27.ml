type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match n1 with
  | ZERO ->
    (match n2 with
      | ZERO -> ZERO
      | SUCC n3 -> SUCC(natadd n1 n3))
  | SUCC n4 -> SUCC(natadd n4 n2);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
match n1 with
  | ZERO -> ZERO
  | SUCC n3 -> natadd (natmul n3 n2) n2;;


let two = SUCC(SUCC ZERO);;
let three = SUCC(SUCC(SUCC ZERO));;
natadd two three;;
natmul two three;;


