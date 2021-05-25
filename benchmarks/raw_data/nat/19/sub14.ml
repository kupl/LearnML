type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat =
    fun a b ->
      match a with
      | ZERO -> b
      | SUCC a -> SUCC (natadd a b)
  ;;
let rec natmul : nat -> nat -> nat =
    fun a b ->
      match a with
      | ZERO -> ZERO
      | SUCC a -> natadd b (natmul a b)
  ;;
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
  
natmul three three;;