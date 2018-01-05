type nat = ZERO | SUCC of nat

let rec natadd (x,y) = 
    match y with
    | ZERO -> 
        let rec enat n = 
          match n with
          | ZERO -> ZERO
          | SUCC (n2) -> SUCC (enat n2)
        in
        enat x
    | SUCC (y2) -> SUCC ( natadd(x,y2) )
