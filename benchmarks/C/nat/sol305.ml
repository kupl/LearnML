type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
    match n2 with
      SUCC(a) -> natadd (SUCC(n1)) a
      | ZERO -> n1;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let rec nmul augend cnt result=
  match cnt with
    SUCC(a) -> let rec ad res aug =
      match aug with
        SUCC(a) -> ad (SUCC(res)) a
        | ZERO -> res
        in nmul augend a (ad result augend)
    |ZERO -> result
    in nmul n1 n2 ZERO;;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
let four = SUCC(three);;
natadd two four;;
natmul two four;;
