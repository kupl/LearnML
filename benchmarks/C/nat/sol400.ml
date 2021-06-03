type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun x y -> (*TODO*)
  match x with
  |ZERO -> y
  |SUCC z -> SUCC(natadd z y);;

let rec natmul : nat -> nat -> nat
= fun x y -> (*TODO*)
        match x with
        |ZERO -> ZERO
        |SUCC ZERO -> y
        |SUCC z -> natadd y (natmul z y);;
                
(*let two = SUCC (SUCC ZERO);;*)
(*let three = SUCC (SUCC (SUCC ZERO));;*)
(*natmul two three;;*)
(*natadd two three;;*)
(*natmul two ZERO;;*)
(*natadd two ZERO;;*)