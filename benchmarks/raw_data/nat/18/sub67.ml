type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  | ZERO -> n2
  | SUCC t -> natadd t (SUCC n2);;

let natmul : nat -> nat -> nat
= fun n1 n2 -> let temp = n2 in
  let rec funtemp p q = match p with
    | ZERO -> ZERO
    | SUCC ZERO -> q
    | SUCC t -> funtemp t (natadd q temp) in
      funtemp n1 n2;;