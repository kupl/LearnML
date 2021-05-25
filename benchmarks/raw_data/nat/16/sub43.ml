type nat = ZERO | SUCC of nat

let rec natadd (a : nat) (b : nat) : nat =
   match a with
      ZERO -> b
    | SUCC c -> natadd c (SUCC b)

let rec natmul (a : nat) (b :nat) : nat =
   match a with
     ZERO -> ZERO
   | SUCC c -> natadd b (natmul c b)
