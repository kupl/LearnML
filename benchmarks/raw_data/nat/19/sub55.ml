type nat = ZERO | SUCC of nat

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
  | ZERO -> n2
  | SUCC(m) -> SUCC(natadd m n2);;

let rec _natmul = fun x y z ->
  match z with
    | ZERO -> x
    | SUCC (z') -> (_natmul (natadd x y) y z');;

let natmul = fun x y ->
  match y with
    | ZERO -> ZERO
    | _ -> (_natmul ZERO x y);;