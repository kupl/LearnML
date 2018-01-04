type nat = ZERO | SUCC of nat

let rec natadd ((a: nat), (b: nat)) : nat =
     match a with
     | ZERO -> b
     | SUCC a' -> (natadd (a', SUCC b))

let rec natmul2 ((a: nat), (b: nat), (r: nat)) : nat =
     match a with
     | ZERO -> r
     | a ->
               (match b with
               | ZERO -> r
               | SUCC b' -> (natmul2 (a, b', (natadd (a, r)))))

let natmul ((a: nat), (b: nat)) : nat =
     natmul2 (a, b, ZERO)
