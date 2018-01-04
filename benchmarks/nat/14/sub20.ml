type nat = 
    | ZERO          (* 0 *)
    | SUCC of nat   (* 0/ *)

let rec natadd (n1, n2) = 
    match n1, n2 with
    | ZERO, ZERO -> ZERO
    | ZERO, SUCC a -> SUCC a
    | SUCC a, ZERO -> SUCC a
    | SUCC a, SUCC b -> SUCC(SUCC (natadd (a, b)))


let rec natmul (n3, n4) = 
    match n3, n4 with
    | ZERO, ZERO -> ZERO
    | ZERO, SUCC a -> ZERO
    | SUCC a, ZERO -> ZERO
    | SUCC ZERO, SUCC ZERO -> SUCC ZERO
    | SUCC ZERO, SUCC a -> SUCC a
    | SUCC a, SUCC ZERO -> SUCC a
    | SUCC a, SUCC b -> (natadd (SUCC a, (natmul (SUCC a, b))))



  
