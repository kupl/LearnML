type nat = ZERO | SUCC of nat

let rec realNum n =
  match n with
    | ZERO -> 0
    | SUCC n2 -> 1 + realNum n2;;


let rec natGen cont =
  match cont with
    | 0 -> ZERO
    | _ -> SUCC ( natGen (cont-1));;

let natadd : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  natGen ((realNum n1)+(realNum n2));;

let natmul : nat -> nat -> nat
= fun n1 n2 -> (*TODO*)
  natGen ((realNum n1)*(realNum n2));;
  
