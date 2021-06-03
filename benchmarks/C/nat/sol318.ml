type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> 
    let rec intToNat x = if x == 0 then ZERO else SUCC (intToNat (x - 1)) in
      let rec natToInt n =
        match n with
           ZERO -> 0
          |(SUCC ls) -> 1 + (natToInt ls)
        in intToNat ((natToInt n1) + (natToInt n2));;

let natmul : nat -> nat -> nat
= fun n1 n2 -> 
    let rec intToNat x = if x == 0 then ZERO else SUCC (intToNat (x - 1)) in
      let rec natToInt n =
        match n with
           ZERO -> 0
          |(SUCC ls) -> 1 + (natToInt ls)
        in intToNat ((natToInt n1) * (natToInt n2));;
        
natmul (SUCC (SUCC ZERO)) (SUCC (SUCC (SUCC ZERO)));;