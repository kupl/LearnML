type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
  match n2 with
  | ZERO -> 
      let rec calcnat r = 
        match r with
        | ZERO -> ZERO
        | SUCC (r2) -> SUCC (calcnat r2)
      in
      calcnat n1
  | SUCC (n22) -> SUCC (natadd n1 n22)
;;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
  match n2 with
  | ZERO -> ZERO
  | SUCC(n22) -> natadd n1 (natmul n1 n22)
;;
