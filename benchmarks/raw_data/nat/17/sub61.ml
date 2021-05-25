(*problem 2*)
  type nat = ZERO|SUCC of nat
  let natadd: nat -> nat -> nat
  = fun n1 n2 ->
    let add =
      let rec toInt input =
        match input with
        ZERO -> 0
        |SUCC n -> 1 + (toInt n) in
      toInt n1 + toInt n2 in
    let rec toNat outcome =
      match outcome with
      0 -> ZERO
      |1 -> SUCC ZERO
      |_ -> SUCC (toNat(outcome-1)) in
    toNat add;;

  let natmul: nat -> nat -> nat
  = fun n1 n2 ->
    let mul =
      let rec toInt input =
        match input with
        ZERO -> 0
        |SUCC n -> 1 + (toInt n) in
      toInt n1 * toInt n2 in
    let rec toNat outcome =
      match outcome with
      0 -> ZERO
      |1 -> SUCC ZERO
      |_ -> SUCC (toNat(outcome-1)) in
    toNat mul;;

  let rec natexp: nat -> nat -> nat
  = fun n1 n2 ->    
    let expv =  
       let rec toInt input =
       match input with
       ZERO -> 0
       |SUCC n -> 1 + (toInt n) in
       let rec exp a b =
       if b = 0 then 0
       else a * (exp a (b-1)) in
    exp (toInt n1) (toInt n2) in
      let rec toNat outcome =
        match outcome with
        0 -> ZERO
        |1 -> SUCC ZERO
        |_ -> SUCC (toNat(outcome-1)) in
    toNat expv;;
