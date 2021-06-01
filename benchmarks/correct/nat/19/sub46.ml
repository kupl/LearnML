type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  match n1 with 
    | ZERO -> n2
    | SUCC tl -> SUCC (natadd tl n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
let res = ZERO in 
let rec f n1 res = 
    match n1 with
    | ZERO -> res
    | SUCC tl -> f tl (natadd res n2) in
  f n1 res;;


(*type nat = ZERO | SUCC of nat*)

(*let natadd : nat -> nat -> nat*)
(*= fun n1 n2 -> (*TODO*)*)

(*let natmul : nat -> nat -> nat*)
(*= fun n1 n2 -> (*TODO*)*)
