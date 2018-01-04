type nat = ZERO | SUCC of nat;;

let rec natadd (a,b) = 
    match b with
    | ZERO -> 
        let rec evalnat n = 
          match n with
          | ZERO -> ZERO
          | SUCC (n2) -> SUCC (evalnat n2)
        in
        evalnat a
    | SUCC (b2) -> SUCC ( natadd(a,b2) );;

let rec natmul (a,b) = 
    match b with
    | ZERO -> ZERO
    | SUCC (b2) -> natadd (a, natmul(a,b2));;
