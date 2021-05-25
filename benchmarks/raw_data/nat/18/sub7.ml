type nat =
    ZERO
  | SUCC of nat;;

let rec natadd : nat -> nat -> nat =
  fun n1 n2 ->
    match n1 with
      ZERO -> n2
    | SUCC n -> natadd n (SUCC n2);;
    

let natmul : nat -> nat -> nat =
  fun n1 n2 ->
    let rec inner num1 num2 =
      match num1 with
        ZERO -> ZERO
      | SUCC ZERO -> num2
      | SUCC n -> inner n (natadd num2 n2)
    in
    inner n1 n2;;